{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Fixed
import Data.Function
import Data.IntMap (IntMap)
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector, (!))
import System.Exit
import System.IO

import qualified Data.IntMap as I
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Util.Text (ToText(..), showText)
import Kitten.Yarn (Instruction)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Yarn as Y

data Env = Env
  { envCalls :: !(IORef [Call])
  , envClosures :: !(IORef [Vector InterpreterValue])
  , envData :: !(IORef [InterpreterValue])
  , envInstructions :: !(Vector Instruction)
  , envIp :: !(IORef Ip)
  , envLabels :: !(IntMap Int)
  , envLocation :: !(IORef Location)
  }

data Call = Call !CallType !Int | Local !InterpreterValue
data CallType = WithoutClosure | WithClosure
type Interpret a = ReaderT Env IO a
type Ip = Int
type Offset = Ip -> Ip

interpret
  :: [InterpreterValue]
  -> Vector Instruction
  -> IO [InterpreterValue]
interpret stack instructions = do
  envCalls <- newIORef []
  envClosures <- newIORef []
  envData <- newIORef stack
  let
    envInstructions = instructions
    (envLabels, entryIndex) = let
      go index instruction (labels, entry) = case instruction of
        Y.Label label -> (I.insert label index labels, entry)
        Y.EntryLabel -> (labels, index)
        _ -> (labels, entry)
      in V.ifoldr go (I.empty, error "Missing entry point.") instructions
  envIp <- newIORef entryIndex
  envLocation <- newIORef (newLocation "interpreter" 0 0)
  let env0 = Env{..}
  _ <- fix $ \loop -> do
    offset <- runReaderT interpretInstruction env0
    modifyIORef' envIp offset
    loop
  readIORef envData

interpretInstruction :: Interpret Offset
interpretInstruction = do
  instruction <- do
    instructions <- asks envInstructions
    ip <- asksIO envIp
    let len = V.length instructions
    if ip < 0 || ip >= len
      then error $ concat
        [ "Instruction pointer ("
        , show ip
        , ") out of bounds ("
        , show len
        , ")"
        ]
      else return (instructions ! ip)

  case instruction of
    Y.Act label closure -> do
      values <- V.mapM getClosedName closure
      pushData (Activation label values)
      return succ
    Y.Builtin builtin -> interpretBuiltin builtin
    Y.Call label -> call label Nothing
    Y.Closure index -> do
      pushData =<< getClosed index
      return succ
    Y.Comment{} -> return succ
    Y.Enter -> do
      pushLocal =<< popData
      return succ
    Y.EntryLabel -> return succ
    Y.Jump offset -> return (+ offset)
    Y.JumpIfFalse _offset -> error "TODO jf"
    Y.JumpIfNone _offset -> error "TODO jn"
    Y.JumpIfRight _offset -> error "TODO jr"
    Y.Leave -> popLocal >> return succ
    Y.Label _label -> return succ
    Y.Local index -> (pushData =<< getLocal index) >> return succ
    Y.MakeVector size -> do
      pushData . Vector . V.reverse . V.fromList
        =<< replicateM size popData
      return succ
    Y.Push value -> pushData (interpreterValue value) >> return succ
    Y.Return -> fix $ \loop -> do
      calls <- asksIO envCalls
      case calls of
        -- Discard all locals pushed during this call frame.
        Local _ : rest -> envCalls =: rest >> loop
        Call type_ target : rest -> do
          envIp =: target
          envCalls =: rest
          case type_ of
            WithClosure -> envClosures ~: tail
            _ -> return ()
          return succ
        [] -> lift exitSuccess

interpreterValue :: Y.Value -> InterpreterValue
interpreterValue value = case value of
  Y.Bool x -> Bool x
  Y.Char x -> Char x
  Y.Choice x y -> Choice x (interpreterValue y)
  Y.Float x -> Float x
  Y.Handle x -> Handle x
  Y.Int x -> Int x
  Y.Option x -> Option (interpreterValue <$> x)
  Y.Pair x y -> Pair (interpreterValue x) (interpreterValue y)
  Y.Unit -> Unit
  Y.String x -> Vector . V.fromList $ map Char (T.unpack x)

getClosedName :: ClosedName -> Interpret InterpreterValue
getClosedName (ClosedName (Name name)) = getLocal name
getClosedName (ReclosedName (Name name)) = getClosed name

interpretQuotation :: InterpreterValue -> Interpret Offset
interpretQuotation (Activation label closure) = call label (Just closure)
interpretQuotation _ = error "Attempt to call non-function."

call :: Y.Label -> Maybe (Vector InterpreterValue) -> Interpret Offset
call label mClosure = do
  destination <- (I.! label) <$> asks envLabels
  ip <- asksIO envIp
  case mClosure of
    Nothing -> envCalls ~: (Call WithoutClosure ip :)
    Just closure -> do
      envClosures ~: (closure :)
      envCalls ~: (Call WithClosure ip :)
  return (const destination)

interpretBuiltin :: Builtin -> Interpret Offset
interpretBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat (+)
  Builtin.AddInt -> intsToInt (+)

  Builtin.AddVector -> do
    Vector b <- popData
    Vector a <- popData
    pushData $ Vector (a <> b)
    return succ

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply -> interpretQuotation =<< popData

  Builtin.CharToInt -> do
    Char a <- popData
    pushData $ Int (fromEnum a)
    return succ

  Builtin.Choice -> do
    left <- popData
    Choice which value <- popData
    if which then return succ
      else pushData value >> interpretQuotation left

  Builtin.ChoiceElse -> do
    right <- popData
    left <- popData
    Choice which value <- popData
    pushData value
    interpretQuotation $ if which then right else left

  Builtin.Close -> do
    Handle a <- popData
    lift $ hClose a
    return succ

  Builtin.DivFloat -> floatsToFloat (/)
  Builtin.DivInt -> intsToInt div

  Builtin.EqFloat -> floatsToBool (==)
  Builtin.EqInt -> intsToBool (==)

  Builtin.Exit -> do
    Int a <- popData
    lift $ case a of
      0 -> exitSuccess
      _ -> exitWith (ExitFailure a)

  Builtin.First -> do
    Pair a _ <- popData
    pushData a
    return succ

  Builtin.FromLeft -> do
    Choice False a <- popData
    pushData a
    return succ

  Builtin.FromRight -> do
    Choice True a <- popData
    pushData a
    return succ

  Builtin.FromSome -> do
    Option (Just a) <- popData
    pushData a
    return succ

  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector a <- popData
    pushData . Option $ if b >= 0 && b < V.length a
      then Just (a ! b)
      else Nothing
    return succ

  Builtin.GetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)
    return succ

  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)

  Builtin.If -> do
    true <- popData
    Bool test <- popData
    if test then interpretQuotation true else return succ

  Builtin.IfElse -> do
    false <- popData
    true <- popData
    Bool test <- popData
    interpretQuotation $ if test then true else false

  Builtin.Impure -> return succ

  Builtin.Init -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.init a
    return succ

  Builtin.IntToChar -> do
    Int a <- popData
    pushData . Option $ if a >= 0 && a <= 0x10FFFF
      then Just $ Char (toEnum a)
      else Nothing
    return succ

  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)

  Builtin.Left -> do
    a <- popData
    pushData $ Choice False a
    return succ

  Builtin.Length -> do
    Vector a <- popData
    pushData . Int $ V.length a
    return succ

  Builtin.LtFloat -> floatsToBool (<)
  Builtin.LtInt -> intsToBool (<)

  Builtin.ModFloat -> floatsToFloat mod'
  Builtin.ModInt -> intsToInt mod

  Builtin.MulFloat -> floatsToFloat (*)
  Builtin.MulInt -> intsToInt (*)

  Builtin.NeFloat -> floatsToBool (/=)
  Builtin.NeInt -> intsToBool (/=)

  Builtin.NegFloat -> floatToFloat negate
  Builtin.NegInt -> intToInt negate

  Builtin.None -> pushData (Option Nothing) >> return succ

  Builtin.NotBool -> boolToBool not
  Builtin.NotInt -> intToInt complement

  Builtin.OrBool -> boolsToBool (||)
  Builtin.OrInt -> intsToInt (.|.)

  Builtin.OpenIn -> openFilePushHandle ReadMode
  Builtin.OpenOut -> openFilePushHandle WriteMode

  Builtin.Option -> do
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretQuotation some
      Nothing -> return succ

  Builtin.OptionElse -> do
    none <- popData
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretQuotation some
      Nothing -> interpretQuotation none

  Builtin.Pair -> do
    b <- popData
    a <- popData
    pushData $ Pair a b
    return succ

  Builtin.Print -> do
    Handle b <- popData
    Vector a <- popData
    lift $ hPutStr b (stringFromChars a) >> hFlush b
    return succ

  Builtin.Rest -> do
    Pair _ b <- popData
    pushData b
    return succ

  Builtin.Right -> do
    a <- popData
    pushData $ Choice True a
    return succ

  Builtin.Set -> do
    c <- popData
    Int b <- popData
    Vector a <- popData
    pushData . Vector
      $ let (before, after) = V.splitAt b a
      in before <> V.singleton c <> V.drop 1 after
    return succ

  Builtin.ShowFloat -> do
    Float value <- popData
    pushData $ Vector (charsFromString $ show value)
    return succ

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Vector (charsFromString $ show value)
    return succ

  Builtin.Some -> do
    a <- popData
    pushData $ Option (Just a)
    return succ

  Builtin.Stderr -> pushData (Handle stderr) >> return succ
  Builtin.Stdin -> pushData (Handle stdin) >> return succ
  Builtin.Stdout -> pushData (Handle stdout) >> return succ

  Builtin.SubFloat -> floatsToFloat (-)
  Builtin.SubInt -> intsToInt (-)

  Builtin.Tail -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.tail a
    return succ

  Builtin.UnsafePurify11 -> return succ

  Builtin.XorBool -> boolsToBool (/=)

  Builtin.XorInt -> intsToInt xor

  where

  boolToBool :: (Bool -> Bool) -> Interpret Offset
  boolToBool f = do
    Bool a <- popData
    pushData $ Bool (f a)
    return succ

  boolsToBool :: (Bool -> Bool -> Bool) -> Interpret Offset
  boolsToBool f = do
    Bool b <- popData
    Bool a <- popData
    pushData $ Bool (f a b)
    return succ

  floatToFloat :: (Double -> Double) -> Interpret Offset
  floatToFloat f = do
    Float a <- popData
    pushData $ Float (f a)
    return succ

  floatsToBool :: (Double -> Double -> Bool) -> Interpret Offset
  floatsToBool f = do
    Float b <- popData
    Float a <- popData
    pushData $ Bool (f a b)
    return succ

  floatsToFloat :: (Double -> Double -> Double) -> Interpret Offset
  floatsToFloat f = do
    Float b <- popData
    Float a <- popData
    pushData $ Float (f a b)
    return succ

  intToInt :: (Int -> Int) -> Interpret Offset
  intToInt f = do
    Int a <- popData
    pushData $ Int (f a)
    return succ

  intsToBool :: (Int -> Int -> Bool) -> Interpret Offset
  intsToBool f = do
    Int b <- popData
    Int a <- popData
    pushData $ Bool (f a b)
    return succ

  intsToInt :: (Int -> Int -> Int) -> Interpret Offset
  intsToInt f = do
    Int b <- popData
    Int a <- popData
    pushData $ Int (f a b)
    return succ

  openFilePushHandle :: IOMode -> Interpret Offset
  openFilePushHandle ioMode = do
    Vector a <- popData
    let fileName = stringFromChars a
    handle <- lift $ openFile fileName ioMode
    pushData $ Handle handle
    return succ

data InterpreterValue
  = Activation !Y.Label !(Vector InterpreterValue)
  | Bool !Bool
  | Char !Char
  | Choice !Bool !InterpreterValue
  | Float !Double
  | Handle !Handle
  | Int !Int
  | Option !(Maybe InterpreterValue)
  | Pair !InterpreterValue !InterpreterValue
  | Unit
  | Vector !(Vector InterpreterValue)
  | Wrapped !Text !InterpreterValue

instance Show InterpreterValue where
  show = T.unpack . toText

instance ToText InterpreterValue where
  toText value = case value of
    Activation label _ -> "<function@" <> showText label <> ">"
    Bool b -> if b then "true" else "false"
    Char c -> showText c
    Choice which v -> T.unwords
      [toText v, if which then "right" else "left"]
    Float f -> showText f
    Handle{} -> "<handle>"
    Int i -> showText i
    Option m -> maybe "none" ((<> " some") . toText) m
    Pair a b -> T.concat ["(", toText a, ", ", toText b, ")"]
    Unit -> "()"
    Vector v@(V.toList -> (Char _ : _)) -> showText (stringFromChars v)
    Vector v -> T.concat
      [ "["
      , T.intercalate ", " (V.toList (V.map toText v))
      , "]"
      ]
    Wrapped name v -> T.unwords [toText v, "to", name]

charsFromString :: String -> Vector InterpreterValue
charsFromString = V.fromList . map Char

getClosed :: Int -> Interpret InterpreterValue
getClosed index = do
  closure : _ <- asksIO envClosures
  return $ closure ! index

getLocal :: Int -> Interpret InterpreterValue
getLocal index = do
  locals <- asksIO envCalls
  case locals !! index of
    Local value -> return value
    _ -> error "Bad local variable access."

asksIO :: (Env -> IORef r) -> Interpret r
asksIO view = do
  ref <- asks view
  lift $ readIORef ref

(~:) :: (Env -> IORef r) -> (r -> r) -> Interpret ()
view ~: f = do
  ref <- asks view
  lift $ modifyIORef' ref f
infix 4 ~:

(=:) :: (Env -> IORef r) -> r -> Interpret ()
view =: value = do
  ref <- asks view
  lift $ writeIORef ref value
infix 4 =:

popData :: Interpret InterpreterValue
popData = do
  dataStack <- asksIO envData
  case dataStack of
    [] -> error "Data stack underflow."
    (top : down) -> envData =: down >> return top

popLocal :: Interpret ()
popLocal = do
  localStack <- asksIO envCalls
  case localStack of
    Local _ : down -> envCalls =: down
    _ -> error "Local stack underflow."

pushData :: InterpreterValue -> Interpret ()
pushData value = envData ~: (value :)

pushLocal :: InterpreterValue -> Interpret ()
pushLocal value = envCalls ~: (Local value :)

stringFromChars :: Vector InterpreterValue -> String
stringFromChars = V.toList . V.map fromChar
  where
  fromChar :: InterpreterValue -> Char
  fromChar (Char c) = c
  fromChar _ = error "stringFromChars: non-character"
