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
  , popData
  , popLocal
  , pushData
  , pushLocal
  , stringFromChars
  , typeOfValue
  , withClosure
  ) where

import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid
import Data.Vector (Vector, (!))
import System.Exit
import System.IO

import qualified Data.Vector as V

import Kitten.Def
import Kitten.Location
import Kitten.Name
import Kitten.Tree (TypedTerm)
import Kitten.Type (Type((:&), (:?), (:|)))
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Tree as Tree
import qualified Kitten.Type as Type
import qualified Kitten.Util.Text as T
import qualified Kitten.Util.Vector as V

data Env = Env
  { envData :: [InterpreterValue]
  , envLocals :: [InterpreterValue]
  , envDefs :: !(Vector (Def TypedTerm))
  , envClosure :: !(Vector InterpreterValue)
  }

type Interpret = InterpretM ()

type InterpretM a = StateT Env IO a

data InterpreterValue
  = Activation !(Vector InterpreterValue) !TypedTerm
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
    Activation{} -> "<function>"
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

getClosed :: Name -> InterpretM InterpreterValue
getClosed (Name index) = do
  closure <- gets envClosure
  return $ closure ! index

getLocal :: Name -> InterpretM InterpreterValue
getLocal (Name index) = do
  locals <- gets envLocals
  return $ locals !! index

popData :: InterpretM InterpreterValue
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> do
      lift $ do
        hPutStrLn stderr "stack underflow"
        exitFailure
    (top : down) -> do
      modify $ \env -> env { envData = down }
      return top

popLocal :: Interpret
popLocal = do
  localStack <- gets envLocals
  case localStack of
    [] -> do
      lift $ do
        hPutStrLn stderr "local stack underflow"
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

typeOfValue
  :: Location -> InterpreterValue -> NameGen -> (Type Type.Scalar, NameGen)
typeOfValue loc = runState . typeOfValueM loc

typeOfValueM
  :: Location -> InterpreterValue -> State NameGen (Type Type.Scalar)
typeOfValueM loc value = case value of
  Activation _closed typed -> return $ Tree.typedType typed
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
  recur = typeOfValueM loc

  freshVarM :: State NameGen (Type a)
  freshVarM = do
    name <- state genName
    return $ Type.Var (Type.TypeName name) origin

  -- TODO(strager): Type hint for stack elements.
  origin :: Type.Origin
  origin = Type.Origin Type.NoHint loc

withClosure :: Vector InterpreterValue -> InterpretM a -> InterpretM a
withClosure values action = do
  closure <- gets envClosure
  modify $ \env -> env { envClosure = values }
  result <- action
  modify $ \env -> env { envClosure = closure }
  return result
