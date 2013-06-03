{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck.Monad
  ( Env(..)
  , Typecheck
  , emptyEnv
  , evalTypecheck
  , getEnv
  , getLocal
  , here
  , getsEnv
  , guardLiftM2
  , guardMapM
  , guardReturn
  , halt
  , hypothetically
  , hypothetically'
  , internalError
  , modifyEnv
  , popData
  , popDataExpecting
  , popDataExpecting_
  , pushData
  , pushLocal
  , putEnv
  , stackEffect
  , typeError
  , unify
  , unknownTypeError
  , withLocation
  ) where

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Kitten.Def
import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Resolved
import Kitten.Type
import Kitten.Util.FailWriter (FailWriterT, runFailWriterT)
import Kitten.Util.List

import qualified Kitten.Util.FailWriter as FailWriter

newtype Typecheck a = Typecheck
  { unTypecheck :: FailWriterT [CompileError] (State Env) a }
  deriving (Functor, Applicative, Monad)

data Env = Env
  { envData :: [Type Scalar]
  , envLocals :: [Type Scalar]
  , envDefs :: [Def Resolved]
  , envLocations :: [Location]
  }

-- | Halts typechecking with a compilation error.
compileError :: CompileError -> Typecheck a
compileError err = compileErrors [err]

-- | Halts typechecking with multiple compilation errors.
compileErrors :: [CompileError] -> Typecheck a
compileErrors = Typecheck . FailWriter.throwMany

emptyEnv :: Env
emptyEnv = Env
  { envData = []
  , envLocals = []
  , envDefs = []
  , envLocations = []
  }

evalTypecheck :: Env -> Typecheck a -> Either [CompileError] a
evalTypecheck env action
  = fst $ runTypecheck env action

getEnv :: Typecheck Env
getEnv = Typecheck $ lift get

getLocal :: Name -> Typecheck (Type Scalar)
getLocal (Name index) = getsEnv ((!! index) . envLocals)

getsEnv :: (Env -> a) -> Typecheck a
getsEnv = Typecheck . lift . gets

guardLiftM2 :: (a -> b -> c) -> Typecheck a -> Typecheck b -> Typecheck c
guardLiftM2 f (Typecheck a) (Typecheck b)
  = Typecheck $ FailWriter.guardLiftM2 f a b

guardMapM :: (a -> Typecheck b) -> [a] -> Typecheck [b]
guardMapM f xs
  = Typecheck $ FailWriter.guardMapM (unTypecheck . f) xs

-- | Accumulates compile errors and resumes typechecking.
guardReturn :: Typecheck a -> Typecheck (Maybe a)
guardReturn = Typecheck . FailWriter.guardReturn . unTypecheck

halt :: Typecheck a
halt = Typecheck FailWriter.halt

here :: Typecheck Location
here = do
  locations <- getsEnv envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

hypothetically :: Typecheck a -> Typecheck (Either [CompileError] a, Env)
hypothetically action = do
  env <- getEnv
  return $ runTypecheck env action

hypothetically' :: Typecheck a -> Typecheck (a, Env)
hypothetically' action = do
  (mResult, env) <- hypothetically action
  case mResult of
    Left errs -> compileErrors errs
    Right result -> return (result, env)

internalError :: String -> Typecheck a
internalError = compileError . InternalError

modifyEnv :: (Env -> Env) -> Typecheck ()
modifyEnv = Typecheck . lift . modify

popData :: Typecheck (Type Scalar)
popData = do
  dataStack <- getsEnv envData
  case dataStack of
    [] -> typeError "unexpected empty stack"
    (top : down) -> do
      modifyEnv $ \ env -> env { envData = down }
      return top

popDataExpecting :: Type Scalar -> Typecheck (Type Scalar)
popDataExpecting type_ = do
  dataStack <- getsEnv envData
  case dataStack of
    [] -> typeError $ unwords
      [ "stack underflow does not match expected type"
      , show type_
      ]
    (top : down) -> do
      modifyEnv $ \ env -> env { envData = down }
      unify top type_
      return top

popDataExpecting_ :: Type Scalar -> Typecheck ()
popDataExpecting_ = void . popDataExpecting

pushData :: Type Scalar -> Typecheck ()
pushData type_ = modifyEnv $ \ env@Env{..}
  -> env { envData = type_ : envData }

pushLocal :: Type Scalar -> Typecheck ()
pushLocal type_ = modifyEnv $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }

putEnv :: Env -> Typecheck ()
putEnv = Typecheck . lift . put

runTypecheck :: Env -> Typecheck a -> (Either [CompileError] a, Env)
runTypecheck env (Typecheck m)
  = runState (runFailWriterT null m) env

stackEffect :: Typecheck a -> Typecheck (Type Scalar)
stackEffect action = do
  before <- getEnv
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
  compileError $ TypeError location message

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
  modifyEnv $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modifyEnv $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
