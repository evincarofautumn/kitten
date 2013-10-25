{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Interpret.Monad
  ( Env(..)
  , Interpret
  , InterpretM
  , InterpreterValue(..)
  , charsFromString
  , getClosed
  , getLocal
  , here
  , popData
  , popLocal
  , pushData
  , pushLocal
  , stringFromChars
  , typeOfValue
  , withClosure
  , withLocation
  ) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector, (!))
import System.Exit
import System.IO

import qualified Data.Vector as V

import Kitten.Location
import Kitten.Name
import Kitten.Type (Type((:&), (:?), (:|)))
import Kitten.Typed (Typed, TypedDef)
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Type as Type
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.Text as T
import qualified Kitten.Util.Vector as V

data Env = Env
  { envData :: [InterpreterValue]
  , envLocals :: [InterpreterValue]
  , envDefs :: !(Vector TypedDef)
  , envClosure :: !(Vector InterpreterValue)
  , envLocations :: [Location]
  }

type Interpret = InterpretM ()

type InterpretM a = StateT Env IO a

data InterpreterValue
  = Activation !(Vector InterpreterValue) !Typed
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
    Activation{} -> "<function>"
    Bool b -> if b then "true" else "false"
    Char c -> showText c
    Choice which v -> T.unwords
      [toText v, if which then "right" else "left"]
    Float f -> showText f
    Handle{} -> "<handle>"
    Int i -> showText i
    Option m -> maybe "none" ((<> " some") . toText) m
    Pair a b -> T.unwords [toText a, toText b, "pair"]
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

getClosed :: Name -> InterpretM InterpreterValue
getClosed (Name index) = do
  closure <- gets envClosure
  return $ closure ! index

getLocal :: Name -> InterpretM InterpreterValue
getLocal (Name index) = do
  locals <- gets envLocals
  return $ locals !! index

here :: InterpretM Location
here = do
  locs <- gets envLocations
  return $ case locs of
    [] -> UnknownLocation
    (loc : _) -> loc

popData :: InterpretM InterpreterValue
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> do
      loc <- here
      lift $ do
        hPutStrLn stderr $ show loc ++ ": stack underflow"
        exitFailure
    (top : down) -> do
      modify $ \env -> env { envData = down }
      return top

popLocal :: Interpret
popLocal = do
  localStack <- gets envLocals
  case localStack of
    [] -> do
      loc <- here
      lift $ do
        hPutStrLn stderr $ show loc ++ ": local stack underflow"
        exitFailure
    (_ : down) -> modify $ \env -> env { envLocals = down }

pushData :: InterpreterValue -> Interpret
pushData value = modify $ \env@Env{..}
  -> env { envData = value : envData }

pushLocal :: InterpreterValue -> Interpret
pushLocal value = modify $ \env@Env{..}
  -> env { envLocals = value : envLocals }

stringFromChars :: Vector InterpreterValue -> String
stringFromChars = V.toList . V.map fromChar
  where
  fromChar :: InterpreterValue -> Char
  fromChar (Char c) = c
  fromChar _ = error "stringFromChars: non-character"

typeOfValue :: InterpreterValue -> NameGen -> (Type Type.Scalar, NameGen)
typeOfValue = runState . typeOfValueM

typeOfValueM :: InterpreterValue -> State NameGen (Type Type.Scalar)
typeOfValueM value = case value of
  Activation _closed typed -> return $ Typed.typedType typed
  Bool _ -> return $ Type.Bool loc
  Char _ -> return $ Type.Char loc
  Choice False x -> liftM2 (:|) (typeOfValueM x) freshVarM
  Choice True y -> liftM2 (:|) freshVarM (typeOfValueM y)
  Float _ -> return $ Type.Float loc
  Handle _ -> return $ Type.Handle loc
  Int _ -> return $ Type.Int loc
  Option (Just x) -> liftM (:?) (typeOfValueM x)
  Option Nothing -> liftM (:?) freshVarM
  Pair x y -> liftM2 (:&) (typeOfValueM x) (typeOfValueM y)
  Unit -> return $ Type.Unit loc
  Vector xs -> case V.safeHead xs of
    Nothing -> liftM2 Type.Vector freshVarM (return loc)
    Just x -> liftM2 Type.Vector (typeOfValueM x) (return loc)
  Wrapped name _ -> return $ Type.Named name loc
  where
  freshVarM :: State NameGen (Type a)
  freshVarM = do
    name <- state genName
    return $ Type.Var (Type.TypeName name) loc
  loc :: Location
  loc = UnknownLocation

withClosure :: Vector InterpreterValue -> InterpretM a -> InterpretM a
withClosure values action = do
  closure <- gets envClosure
  modify $ \env -> env { envClosure = values }
  result <- action
  modify $ \env -> env { envClosure = closure }
  return result

withLocation :: Location -> InterpretM a -> InterpretM a
withLocation loc action = do
  modify $ \env@Env{..} -> env
    { envLocations = loc : envLocations }
  result <- action
  modify $ \env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
