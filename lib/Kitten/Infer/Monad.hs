{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Env(..)
  , Inferred(..)
  , Declare(..)
  , emptyEnv
  , freshNameM
  , freshVar
  , freshVarM
  , getEnv
  , getsEnv
  , modifyEnv
  , putEnv
  , retrieve
  , runInference
  , withLocation
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Map (Map)

import qualified Data.Map as Map

import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Type
import Kitten.Util.FailWriter
import Kitten.Util.Maybe

data Env = Env
  { envClosure :: [Type Scalar]
  , envDefs :: Map Name Scheme
  , envLocals :: [Type Scalar]
  , envLocation :: Location
  , envNext  :: Name
  , envRows :: Map Name (Type Row)
  , envScalars :: Map Name (Type Scalar)
  } deriving (Show)

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [CompileError] (State Env) a
  } deriving (Applicative, Functor, Monad)

class Declare a where
  declare :: TypeName a -> Type a -> Env -> Env

instance Declare Row where
  declare (TypeName name) type_ env = env
    { envRows = Map.insert name type_ (envRows env) }

instance Declare Scalar where
  declare (TypeName name) type_ env = env
    { envScalars = Map.insert name type_ (envScalars env) }

emptyEnv :: Env
emptyEnv = Env
  { envClosure = []
  , envDefs = Map.empty
  , envLocals = []
  , envLocation = UnknownLocation
  , envNext = Name 0
  , envRows = Map.empty
  , envScalars = Map.empty
  }

class Retrieve a where
  retrieve :: Env -> TypeName a -> Either CompileError (Type a)

instance Retrieve Row where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (Map.lookup name envRows)
    $ TypeError envLocation $ unwords
    [ "nonexistent row type variable:"
    , show name
    ]

instance Retrieve Scalar where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (Map.lookup name envScalars)
    $ TypeError envLocation $ unwords
    [ "nonexistent scalar type variable:"
    , show name
    ]

freshName :: Env -> (Name, Env)
freshName env
  = let current = envNext env
  in (current, env { envNext = succ current })

freshVar :: Env -> (Type a, Env)
freshVar env
  = let (name, env') = freshName env
  in (Var name, env')

freshNameM :: Inferred Name
freshNameM = Inferred . lift $ state freshName

freshVarM :: Inferred (Type a)
freshVarM = Inferred . lift $ state freshVar

getEnv :: Inferred Env
getEnv = Inferred $ lift get

getsEnv :: (Env -> a) -> Inferred a
getsEnv = Inferred . lift . gets

modifyEnv :: (Env -> Env) -> Inferred ()
modifyEnv = Inferred . lift . modify

putEnv :: Env -> Inferred ()
putEnv = Inferred . lift . put

runInference
  :: Env
  -> Inferred a
  -> (Either [CompileError] a, Env)
runInference env action = flip runState env
  . runFailWriterT null $ unwrapInferred action

withLocation :: Location -> Inferred a -> Inferred a
withLocation here action = do
  there <- getsEnv envLocation
  modifyEnv $ \ env -> env { envLocation = here }
  result <- action
  modifyEnv $ \ env -> env { envLocation = there }
  return result
