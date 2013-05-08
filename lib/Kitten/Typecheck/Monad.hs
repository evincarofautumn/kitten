{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck.Monad
  ( Env(..)
  , Typecheck
  , TypecheckM
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
import Kitten.Resolve
import Kitten.Type
import Kitten.Util.List

type Typecheck = TypecheckM ()

type TypecheckM a = StateT Env (Either CompileError) a

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

getLocal :: Name -> TypecheckM (Type Scalar)
getLocal (Name index) = gets ((!! index) . envLocals)

here :: TypecheckM Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

hypothetically :: TypecheckM a -> TypecheckM (Env, Env)
hypothetically action = do
  before <- get
  void action
  after <- get
  put before
  return (before, after)

internalError :: String -> TypecheckM a
internalError = lift . Left . InternalError

popData :: TypecheckM (Type Scalar)
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> typeError "unexpected empty stack"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popDataExpecting :: Type Scalar -> TypecheckM (Type Scalar)
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

popDataExpecting_ :: Type Scalar -> Typecheck
popDataExpecting_ = void . popDataExpecting

pushData :: Type Scalar -> Typecheck
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

pushLocal :: Type Scalar -> Typecheck
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }

stackEffect :: TypecheckM a -> TypecheckM (Type Scalar)
stackEffect action = do
  (before, after) <- hypothetically action
  let
    stackBefore = envData before
    stackAfter = envData after
    (consumption, production) = stripCommonPrefix
      (reverse stackBefore)
      (reverse stackAfter)
  return
    $ Composition (reverse consumption)
    :> Composition (reverse production)

typeError :: String -> TypecheckM a
typeError message = do
  location <- here
  lift . Left $ TypeError location message

unify
  :: Type Scalar
  -> Type Scalar
  -> Typecheck
unify actual expected
  = when (actual /= expected) . typeError $ unwords
  [ "expected"
  , show expected
  , "but got"
  , show actual
  ]

unknownTypeError :: Typecheck
unknownTypeError = internalError
  "unknown type appeared during typechecking"

withLocation :: Location -> TypecheckM a -> TypecheckM a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
