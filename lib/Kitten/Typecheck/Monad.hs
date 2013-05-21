{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck.Monad
  ( Env(..)
  , Typecheck
  , emptyEnv
  , getLocal
  , hypothetically
  , internalError
  , popData
  , popDataExpecting
  , popDataExpecting_
  , pushData
  , pushLocal
  , stackEffect
  , typeError
  , unify
  , unknownTypeError
  , withLocation
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Kitten.Def
import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Resolved
import Kitten.Type
import Kitten.Util.List

type Typecheck a = StateT Env (Either CompileError) a

data Env = Env
  { envData :: [Type Scalar]
  , envLocals :: [Type Scalar]
  , envDefs :: [Def Resolved]
  , envLocations :: [Location]
  }

emptyEnv :: Env
emptyEnv = Env
  { envData = []
  , envLocals = []
  , envDefs = []
  , envLocations = []
  }

getLocal :: Name -> Typecheck (Type Scalar)
getLocal (Name index) = gets ((!! index) . envLocals)

here :: Typecheck Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

hypothetically :: Typecheck a -> Typecheck (a, Env)
hypothetically action = do
  before <- get
  result <- action
  after <- get
  put before
  return (result, after)

internalError :: String -> Typecheck a
internalError = lift . Left . InternalError

popData :: Typecheck (Type Scalar)
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> typeError "unexpected empty stack"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popDataExpecting :: Type Scalar -> Typecheck (Type Scalar)
popDataExpecting type_ = do
  dataStack <- gets envData
  case dataStack of
    [] -> typeError $ unwords
      [ "stack underflow does not match expected type"
      , show type_
      ]
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      unify top type_
      return top

popDataExpecting_ :: Type Scalar -> Typecheck ()
popDataExpecting_ = void . popDataExpecting

pushData :: Type Scalar -> Typecheck ()
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

pushLocal :: Type Scalar -> Typecheck ()
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }

stackEffect :: Typecheck a -> Typecheck (Type Scalar)
stackEffect action = do
  before <- get
  (_, after) <- hypothetically action
  let
    stackBefore = envData before
    stackAfter = envData after
    (consumption, production) = stripCommonPrefix
      (reverse stackBefore)
      (reverse stackAfter)
  return
    $ Composition (reverse consumption)
    :> Composition (reverse production)

typeError :: String -> Typecheck a
typeError message = do
  location <- here
  lift . Left $ TypeError location message

unify
  :: Type Scalar
  -> Type Scalar
  -> Typecheck ()
unify actual expected
  = when (actual /= expected) . typeError $ unwords
  [ "expected"
  , show expected
  , "but got"
  , show actual
  ]

unknownTypeError :: Typecheck a
unknownTypeError = internalError
  "unknown type appeared during typechecking"

withLocation :: Location -> Typecheck a -> Typecheck a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
