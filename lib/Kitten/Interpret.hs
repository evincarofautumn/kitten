{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Interpret
  ( InterpreterValue(..)
  , interpret
  , typeOf
  ) where

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
import Data.Vector (Vector, (!))
import System.Exit
import System.IO
import Text.Printf hiding (fromChar)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.IR
import Kitten.Id
import Kitten.IdMap (DefIdMap)
import Kitten.Intrinsic
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Name
import Kitten.Program
import Kitten.Type
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.IdMap as Id
import qualified Kitten.Util.Vector as V

data Env = Env
  { envCalls :: !(IORef [FrameEntry])
  , envClosures :: !(IORef [Vector InterpreterValue])
  , envData :: !(IORef [InterpreterValue])
  , envInstructions :: !(DefIdMap IrBlock)
  , envIp :: !(IORef Ip)
  , envLocation :: !(IORef Location)
  , envSymbols :: !(DefIdMap [Name])
  }

data FrameEntry
  = FrCall !CallType !Ip
  | FrLocal !InterpreterValue

data CallType = WithoutClosure | WithClosure
  deriving (Eq)

type Interpret a = ReaderT Env IO a
type Ip = (DefId, Int)
type Offset = Maybe (Ip -> Ip)

interpret
  :: Maybe Int
  -> [InterpreterValue]
  -> Program
  -> IO [InterpreterValue]
interpret mStart stack program = do
  envCalls <- newIORef []
  envClosures <- newIORef []
  envData <- newIORef stack
  envIp <- newIORef (entryId, fromMaybe 0 mStart)
  envLocation <- newIORef (newLocation "interpreter" 0 0)
  let
    envInstructions = programBlocks program
    envSymbols = inverseSymbols program
    env0 = Env{..}
  fix $ \loop -> do
    offset <- flip runReaderT env0 $ do
      mInstruction <- currentInstruction
      case mInstruction of
        Just instruction -> interpretInstruction instruction
        Nothing -> return Nothing
    case offset of
      Just offset' -> modifyIORef' envIp offset' >> loop
      Nothing -> noop
  readIORef envData

-- TODO Make this not do a lookup every step.
currentInstruction :: Interpret (Maybe IrInstruction)
currentInstruction = do
  instructions <- asks envInstructions
  (defId, ip) <- asksIO envIp
  let def = instructions Id.! defId
  let len = V.length def
  return $ if ip >= len
    then Nothing
    else Just (def ! ip)

interpretInstruction :: IrInstruction -> Interpret Offset
interpretInstruction instruction = case instruction of
  IrAct (target, closure, type_) -> do
    values <- V.mapM getClosedName closure
    pushData (Activation target values type_)
    proceed
  IrIntrinsic intrinsic -> interpretIntrinsic intrinsic
  IrCall label -> call label Nothing
  IrClosure index -> do
    pushData =<< getClosed index
    proceed
  IrConstruct index size type_ -> do
    let user closure = User index closure type_
    pushData . user . V.reverse =<< V.replicateM size popData
    proceed
  IrDrop size -> do
    replicateM_ size popData
    proceed
  IrEnter locals -> do
    replicateM_ locals (pushLocal =<< popData)
    proceed
  IrLabel{} -> proceed
  IrLeave locals -> replicateM_ locals popLocal >> proceed
  IrLocal index -> (pushData =<< getLocal index) >> proceed
  IrMakeVector size -> do
    pushData . Vector . V.reverse =<< V.replicateM size popData
    proceed
  IrMatch cases mDefault -> do
    User index fields _ <- popData
    case V.find (\ (IrCase index' _) -> index == index') cases of
      Just (IrCase _ (target, closure, type_)) -> do
        V.mapM_ pushData fields
        values <- V.mapM getClosedName closure
        interpretQuotation (Activation target values type_)
      Nothing -> case mDefault of
        Just (target, closure, type_) -> do
          values <- V.mapM getClosedName closure
          interpretQuotation (Activation target values type_)
        Nothing -> lift $ do
          hPutStrLn stderr "pattern match failure"
          exitWith (ExitFailure 1)
  IrPush value -> pushData (interpreterValue value) >> proceed
  IrReturn _ -> fix $ \loop -> do
    calls <- asksIO envCalls
    case calls of
      -- Discard all locals pushed during this call frame.
      FrLocal _ : rest -> envCalls =: rest >> loop
      FrCall type_ target : rest -> do
        envIp =: target
        envCalls =: rest
        when (type_ == WithClosure) $ envClosures ~: tail
        proceed
      [] -> return Nothing
  IrTailApply locals -> do
    replicateM_ locals popLocal
    quotation <- popData
    case quotation of
      Activation target closure _ -> do
        envClosures ~: (closure :) . tail
        return $ Just (const (target, 0))
      _ -> error "Attempt to tail-apply non-function."
  IrTailCall locals label -> do
    replicateM_ locals popLocal
    return $ Just (const (label, 0))

interpreterValue :: IrValue -> InterpreterValue
interpreterValue value = case value of
  IrBool x -> Bool x
  IrChar x -> Char x
  IrChoice x y -> Choice x (interpreterValue y)
  IrFloat x -> Float x
  IrInt x -> Int x
  IrOption x -> Option (interpreterValue <$> x)
  IrPair x y -> Pair (interpreterValue x) (interpreterValue y)
  IrString x -> Vector . V.fromList $ map Char (T.unpack x)

getClosedName :: ClosedName -> Interpret InterpreterValue
getClosedName (ClosedName index) = getLocal index
getClosedName (ReclosedName index) = getClosed index

interpretQuotation :: InterpreterValue -> Interpret Offset
interpretQuotation (Activation target closure _) = call target (Just closure)
interpretQuotation _ = error "Attempt to call non-function."

call :: DefId -> Maybe (Vector InterpreterValue) -> Interpret Offset
call target mClosure = do
  ip <- asksIO envIp
  case mClosure of
    Nothing -> envCalls ~: (FrCall WithoutClosure ip :)
    Just closure -> do
      envClosures ~: (closure :)
      envCalls ~: (FrCall WithClosure ip :)
  return $ Just (const (target, 0))

interpretIntrinsic :: Intrinsic -> Interpret Offset
interpretIntrinsic intrinsic = case intrinsic of
  InAddFloat -> floatsToFloat (+)
  InAddInt -> intsToInt (+)

  InAddVector -> do
    Vector b <- popData
    Vector a <- popData
    pushData $ Vector (a <> b)
    proceed

  InAndBool -> boolsToBool (&&)

  InAndInt -> intsToInt (.&.)

  InApply -> interpretQuotation =<< popData

  InCharToInt -> do
    Char a <- popData
    pushData $ Int (fromEnum a)
    proceed

  InChoice -> do
    left <- popData
    Choice which value <- popData
    if which then proceed
      else pushData value >> interpretQuotation left

  InChoiceElse -> do
    right <- popData
    left <- popData
    Choice which value <- popData
    pushData value
    interpretQuotation $ if which then right else left

  InClose -> do
    Handle a <- popData
    lift $ hClose a
    proceed

  InDivFloat -> floatsToFloat (/)
  InDivInt -> intsToInt div

  InEqFloat -> floatsToBool (==)
  InEqInt -> intsToBool (==)

  InExit -> do
    Int a <- popData
    lift $ case a of
      0 -> exitSuccess
      _ -> exitWith (ExitFailure a)

  InFirst -> do
    Pair a _ <- popData
    pushData a
    proceed

  InFloatToInt -> do
    Float a <- popData
    -- TODO: handle corner cases
    pushData $ Option $ Just $ Int $ truncate a
    proceed

  InFromLeft -> do
    Choice False a <- popData
    pushData a
    proceed

  InFromRight -> do
    Choice True a <- popData
    pushData a
    proceed

  InFromSome -> do
    Option (Just a) <- popData
    pushData a
    proceed

  InGeFloat -> floatsToBool (>=)
  InGeInt -> intsToBool (>=)

  InGet -> do
    Int b <- popData
    Vector a <- popData
    pushData . Option $ if b >= 0 && b < V.length a
      then Just (a ! b)
      else Nothing
    proceed

  InGetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)
    proceed

  InGtFloat -> floatsToBool (>)
  InGtInt -> intsToBool (>)

  InIf -> do
    true <- popData
    Bool test <- popData
    if test then interpretQuotation true else proceed

  InIfElse -> do
    false <- popData
    true <- popData
    Bool test <- popData
    interpretQuotation $ if test then true else false

  InInit -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.init a
    proceed

  InIntToChar -> do
    Int a <- popData
    pushData . Option $ if a >= 0 && a <= 0x10FFFF
      then Just $ Char (toEnum a)
      else Nothing
    proceed

  InIntToFloat -> do
    Int a <- popData
    -- TODO: handle corner cases
    pushData $ Option $ Just $ Float $ fromIntegral a
    proceed

  InLeFloat -> floatsToBool (<=)
  InLeInt -> intsToBool (<=)

  InLeft -> do
    a <- popData
    pushData $ Choice False a
    proceed

  InLength -> do
    Vector a <- popData
    pushData . Int $ V.length a
    proceed

  InLtFloat -> floatsToBool (<)
  InLtInt -> intsToBool (<)

  InModFloat -> floatsToFloat mod'
  InModInt -> intsToInt mod

  InMulFloat -> floatsToFloat (*)
  InMulInt -> intsToInt (*)

  InNeFloat -> floatsToBool (/=)
  InNeInt -> intsToBool (/=)

  InNegFloat -> floatToFloat negate
  InNegInt -> intToInt negate

  InNone -> pushData (Option Nothing) >> proceed

  InNotBool -> boolToBool not
  InNotInt -> intToInt complement

  InOrBool -> boolsToBool (||)
  InOrInt -> intsToInt (.|.)

  InOpenIn -> openFilePushHandle ReadMode
  InOpenOut -> openFilePushHandle WriteMode

  InOption -> do
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretQuotation some
      Nothing -> proceed

  InOptionElse -> do
    none <- popData
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretQuotation some
      Nothing -> interpretQuotation none

  InPair -> do
    b <- popData
    a <- popData
    pushData $ Pair a b
    proceed

  InPowFloat -> floatsToFloat (**)

  InPrint -> do
    Handle b <- popData
    Vector a <- popData
    lift $ hPutStr b (stringFromChars a) >> hFlush b
    proceed

  InRest -> do
    Pair _ b <- popData
    pushData b
    proceed

  InRight -> do
    a <- popData
    pushData $ Choice True a
    proceed

  InSet -> do
    c <- popData
    Int b <- popData
    Vector a <- popData
    pushData . Vector
      $ let (before, after) = V.splitAt b a
      in before <> V.singleton c <> V.drop 1 after
    proceed

  InShowFloat -> do
    Float value <- popData
    pushData $ Vector (charsFromString $ printf "%.6f" value)
    proceed

  InShowInt -> do
    Int value <- popData
    pushData $ Vector (charsFromString $ show value)
    proceed

  InSome -> do
    a <- popData
    pushData $ Option (Just a)
    proceed

  InStderr -> pushData (Handle stderr) >> proceed
  InStdin -> pushData (Handle stdin) >> proceed
  InStdout -> pushData (Handle stdout) >> proceed

  InSubFloat -> floatsToFloat (-)
  InSubInt -> intsToInt (-)

  InTail -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.tail a
    proceed

  InXorBool -> boolsToBool (/=)

  InXorInt -> intsToInt xor

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
proceed = return $ Just (fmap succ)

data InterpreterValue
  = Activation !DefId !(Vector InterpreterValue) !(Type 'Scalar)
  | Bool !Bool
  | Char !Char
  | Choice !Bool !InterpreterValue
  | Float !Double
  | Handle !Handle
  | Int !Int
  | Option !(Maybe InterpreterValue)
  | Pair !InterpreterValue !InterpreterValue
  | Vector !(Vector InterpreterValue)
  | User !Int !(Vector InterpreterValue) !(Type 'Scalar)

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
    User index fields _ -> T.concat
      [ T.unwords . V.toList $ V.map toText fields
      , "<data ", showText index, " ", showText $ V.length fields, ">"
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
    FrLocal value -> return value
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
    FrLocal _ : down -> envCalls =: down
    _ -> error "Local stack underflow."

pushData :: InterpreterValue -> Interpret ()
pushData value = envData ~: (value :)

pushLocal :: InterpreterValue -> Interpret ()
pushLocal value = envCalls ~: (FrLocal value :)

stringFromChars :: Vector InterpreterValue -> String
stringFromChars = V.toList . V.map fromChar
  where
  fromChar :: InterpreterValue -> Char
  fromChar (Char c) = c
  fromChar _ = error "stringFromChars: non-character"

typeOf
  :: Location
  -> InterpreterValue
  -> KindedGen 'Scalar
  -> (Type 'Scalar, KindedGen 'Scalar)
typeOf loc = runState . typeOfM loc

typeOfM
  :: Location -> InterpreterValue -> State (KindedGen 'Scalar) (Type 'Scalar)
typeOfM loc value = case value of
  Activation _ _ type_ -> pure type_
  Bool _ -> pure $ tyBool loc
  Char _ -> pure $ tyChar loc
  Choice False x -> TySum <$> recur x <*> freshVarM <*> pure loc
  Choice True y -> TySum <$> freshVarM <*> recur y <*> pure loc
  Float _ -> pure $ tyFloat loc
  Handle _ -> pure $ tyHandle loc
  Int _ -> pure $ tyInt loc
  Option (Just x) -> TyOption <$> recur x <*> pure loc
  Option Nothing -> TyOption <$> freshVarM <*> pure loc
  Pair x y -> TyProduct <$> recur x <*> recur y <*> pure loc
  Vector xs -> case V.safeHead xs of
    Nothing -> TyVector <$> freshVarM <*> pure loc
    Just x -> TyVector <$> recur x <*> pure loc
  User _ _ type_ -> pure type_
  where
  recur = typeOfM loc

  freshVarM :: State (KindedGen 'Scalar) (Type 'Scalar)
  freshVarM = TyVar <$> state genKinded <*> pure loc
