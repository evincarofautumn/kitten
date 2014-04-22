{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Interpret
  ( InterpreterValue(..)
  , interpret
  , typeOf
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Fixed
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector, (!))
import System.Exit
import System.IO

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Id
import Kitten.Location
import Kitten.IdMap (IdMap)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)
import Kitten.Yarn (FlattenedProgram(..), Instruction)
import Kitten.Type (Kind(..), Type((:&), (:?), (:|)))

import qualified Kitten.Builtin as Builtin
import qualified Kitten.IdMap as Id
import qualified Kitten.Type as Type
import qualified Kitten.Util.Vector as V
import qualified Kitten.Yarn as Y

data Env = Env
  { envCalls :: !(IORef [Call])
  , envClosures :: !(IORef [Vector InterpreterValue])
  , envData :: !(IORef [InterpreterValue])
  , envInstructions :: !(Vector Instruction)
  , envIp :: !(IORef Ip)
  , envNames :: !(IdMap Int)
  , envLocation :: !(IORef Location)
  , envSymbols :: !(IdMap [Text])
  }

data Call = Call !CallType !Int | Local !InterpreterValue
data CallType = WithoutClosure | WithClosure
type Interpret a = ReaderT Env IO a
type Ip = Int
type Offset = Maybe (Ip -> Ip)

interpret
  :: Maybe Int
  -> FlattenedProgram
  -> [InterpreterValue]
  -> IO [InterpreterValue]
interpret mIp (FlattenedProgram envInstructions envNames envSymbols) stack = do
  envCalls <- newIORef []
  envClosures <- newIORef []
  envData <- newIORef stack
  envIp <- newIORef $ (envNames Id.! Y.entryName) + fromMaybe 0 mIp
  envLocation <- newIORef (newLocation "interpreter" 0 0)
  let env0 = Env{..}
  fix $ \loop -> do
    offset <- runReaderT interpretInstruction env0
    case offset of
      Just offset' -> modifyIORef' envIp offset' >> loop
      Nothing -> noop
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
    Y.Act label closure type_ -> do
      values <- V.mapM getClosedName closure
      pushData (Activation label values type_)
      proceed
    Y.Builtin builtin -> interpretBuiltin builtin
    Y.Call label -> call label Nothing
    Y.Closure index -> do
      pushData =<< getClosed index
      proceed
    Y.Comment{} -> proceed
    Y.Enter -> do
      pushLocal =<< popData
      proceed
    Y.Leave -> popLocal >> proceed
    Y.Local index -> (pushData =<< getLocal index) >> proceed
    Y.MakeVector size -> do
      pushData . Vector . V.reverse . V.fromList
        =<< replicateM size popData
      proceed
    Y.Push value -> pushData (interpreterValue value) >> proceed
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
            _ -> noop
          proceed
        [] -> return Nothing

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
  Y.String x -> Vector . V.fromList $ map Char (T.unpack x)

getClosedName :: ClosedName -> Interpret InterpreterValue
getClosedName (ClosedName index) = getLocal index
getClosedName (ReclosedName index) = getClosed index

interpretQuotation :: InterpreterValue -> Interpret Offset
interpretQuotation (Activation target closure _) = call target (Just closure)
interpretQuotation _ = error "Attempt to call non-function."

call :: Id -> Maybe (Vector InterpreterValue) -> Interpret Offset
call target mClosure = do
  destination <- (Id.! target) <$> asks envNames
  ip <- asksIO envIp
  case mClosure of
    Nothing -> envCalls ~: (Call WithoutClosure ip :)
    Just closure -> do
      envClosures ~: (closure :)
      envCalls ~: (Call WithClosure ip :)
  return $ Just (const destination)

interpretBuiltin :: Builtin -> Interpret Offset
interpretBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat (+)
  Builtin.AddInt -> intsToInt (+)

  Builtin.AddVector -> do
    Vector b <- popData
    Vector a <- popData
    pushData $ Vector (a <> b)
    proceed

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply -> interpretQuotation =<< popData

  Builtin.CharToInt -> do
    Char a <- popData
    pushData $ Int (fromEnum a)
    proceed

  Builtin.Choice -> do
    left <- popData
    Choice which value <- popData
    if which then proceed
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
    proceed

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
    proceed

  Builtin.FromLeft -> do
    Choice False a <- popData
    pushData a
    proceed

  Builtin.FromRight -> do
    Choice True a <- popData
    pushData a
    proceed

  Builtin.FromSome -> do
    Option (Just a) <- popData
    pushData a
    proceed

  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector a <- popData
    pushData . Option $ if b >= 0 && b < V.length a
      then Just (a ! b)
      else Nothing
    proceed

  Builtin.GetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)
    proceed

  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)

  Builtin.If -> do
    true <- popData
    Bool test <- popData
    if test then interpretQuotation true else proceed

  Builtin.IfElse -> do
    false <- popData
    true <- popData
    Bool test <- popData
    interpretQuotation $ if test then true else false

  Builtin.Impure -> proceed

  Builtin.Init -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.init a
    proceed

  Builtin.IntToChar -> do
    Int a <- popData
    pushData . Option $ if a >= 0 && a <= 0x10FFFF
      then Just $ Char (toEnum a)
      else Nothing
    proceed

  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)

  Builtin.Left -> do
    a <- popData
    pushData $ Choice False a
    proceed

  Builtin.Length -> do
    Vector a <- popData
    pushData . Int $ V.length a
    proceed

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

  Builtin.None -> pushData (Option Nothing) >> proceed

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
      Nothing -> proceed

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
    proceed

  Builtin.Print -> do
    Handle b <- popData
    Vector a <- popData
    lift $ hPutStr b (stringFromChars a) >> hFlush b
    proceed

  Builtin.Rest -> do
    Pair _ b <- popData
    pushData b
    proceed

  Builtin.Right -> do
    a <- popData
    pushData $ Choice True a
    proceed

  Builtin.Set -> do
    c <- popData
    Int b <- popData
    Vector a <- popData
    pushData . Vector
      $ let (before, after) = V.splitAt b a
      in before <> V.singleton c <> V.drop 1 after
    proceed

  Builtin.ShowFloat -> do
    Float value <- popData
    pushData $ Vector (charsFromString $ show value)
    proceed

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Vector (charsFromString $ show value)
    proceed

  Builtin.Some -> do
    a <- popData
    pushData $ Option (Just a)
    proceed

  Builtin.Stderr -> pushData (Handle stderr) >> proceed
  Builtin.Stdin -> pushData (Handle stdin) >> proceed
  Builtin.Stdout -> pushData (Handle stdout) >> proceed

  Builtin.SubFloat -> floatsToFloat (-)
  Builtin.SubInt -> intsToInt (-)

  Builtin.Tail -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.tail a
    proceed

  Builtin.UnsafePurify11 -> proceed

  Builtin.XorBool -> boolsToBool (/=)

  Builtin.XorInt -> intsToInt xor

  where

  boolToBool :: (Bool -> Bool) -> Interpret Offset
  boolToBool f = do
    Bool a <- popData
    pushData $ Bool (f a)
    proceed

  boolsToBool :: (Bool -> Bool -> Bool) -> Interpret Offset
  boolsToBool f = do
    Bool b <- popData
    Bool a <- popData
    pushData $ Bool (f a b)
    proceed

  floatToFloat :: (Double -> Double) -> Interpret Offset
  floatToFloat f = do
    Float a <- popData
    pushData $ Float (f a)
    proceed

  floatsToBool :: (Double -> Double -> Bool) -> Interpret Offset
  floatsToBool f = do
    Float b <- popData
    Float a <- popData
    pushData $ Bool (f a b)
    proceed

  floatsToFloat :: (Double -> Double -> Double) -> Interpret Offset
  floatsToFloat f = do
    Float b <- popData
    Float a <- popData
    pushData $ Float (f a b)
    proceed

  intToInt :: (Int -> Int) -> Interpret Offset
  intToInt f = do
    Int a <- popData
    pushData $ Int (f a)
    proceed

  intsToBool :: (Int -> Int -> Bool) -> Interpret Offset
  intsToBool f = do
    Int b <- popData
    Int a <- popData
    pushData $ Bool (f a b)
    proceed

  intsToInt :: (Int -> Int -> Int) -> Interpret Offset
  intsToInt f = do
    Int b <- popData
    Int a <- popData
    pushData $ Int (f a b)
    proceed

  openFilePushHandle :: IOMode -> Interpret Offset
  openFilePushHandle ioMode = do
    Vector a <- popData
    let fileName = stringFromChars a
    handle <- lift $ openFile fileName ioMode
    pushData $ Handle handle
    proceed

proceed :: Interpret Offset
proceed = return (Just succ)

data InterpreterValue
  = Activation !Id !(Vector InterpreterValue) !(Type Scalar)
  | Bool !Bool
  | Char !Char
  | Choice !Bool !InterpreterValue
  | Float !Double
  | Handle !Handle
  | Int !Int
  | Option !(Maybe InterpreterValue)
  | Pair !InterpreterValue !InterpreterValue
  | Vector !(Vector InterpreterValue)

instance Show InterpreterValue where
  show = T.unpack . toText

instance ToText InterpreterValue where
  toText value = case value of
    Activation label _ _ -> "<function@" <> showText label <> ">"
    Bool b -> if b then "true" else "false"
    Char c -> showText c
    Choice which v -> T.unwords
      [toText v, if which then "right" else "left"]
    Float f -> showText f
    Handle{} -> "<handle>"
    Int i -> showText i
    Option m -> maybe "none" ((<> " some") . toText) m
    Pair a b -> T.concat ["(", toText a, ", ", toText b, ")"]
    Vector v@(V.toList -> (Char _ : _)) -> showText (stringFromChars v)
    Vector v -> T.concat
      [ "["
      , T.intercalate ", " (V.toList (V.map toText v))
      , "]"
      ]

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

typeOf :: Location -> InterpreterValue -> IdGen -> (Type Scalar, IdGen)
typeOf loc = runState . typeOfM loc

typeOfM :: Location -> InterpreterValue -> State IdGen (Type Scalar)
typeOfM loc value = case value of
  Activation _ _ type_ -> return type_
  Bool _ -> return $ Type.bool origin
  Char _ -> return $ Type.char origin
  Choice False x -> liftM2 (:|) (recur x) freshVarM
  Choice True y -> liftM2 (:|) freshVarM (recur y)
  Float _ -> return $ Type.float origin
  Handle _ -> return $ Type.handle origin
  Int _ -> return $ Type.int origin
  Option (Just x) -> liftM (:?) (recur x)
  Option Nothing -> liftM (:?) freshVarM
  Pair x y -> liftM2 (:&) (recur x) (recur y)
  Vector xs -> case V.safeHead xs of
    Nothing -> liftM2 Type.Vector freshVarM (return origin)
    Just x -> liftM2 Type.Vector (recur x) (return origin)
  where
  recur = typeOfM loc

  freshVarM :: State IdGen (Type a)
  freshVarM = do
    i <- state genId
    return $ Type.Var (Type.TypeId i) origin

  -- TODO(strager): Type hint for stack elements.
  origin :: Type.Origin
  origin = Type.Origin Type.NoHint loc
