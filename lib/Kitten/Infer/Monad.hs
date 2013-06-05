{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Env(..)
  , Inferred(..)
  , Instantiation(..)
  , declare
  , emptyEnv
  , findType
  , freshNameM
  , freshVar
  , freshVarM
  , getEnv
  , getsEnv
  , modifyEnv
  , putEnv
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
  { envClosure :: [Type]
  , envDefs :: Map Name Scheme
  , envLocals :: [Type]
  , envLocation :: Location
  , envNext  :: Name
  , envTypes :: Map Name Type
  } deriving (Show)

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [CompileError] (State Env) a
  } deriving (Applicative, Functor, Monad)

data Instantiation = Instantiation
  { instantiationLocation :: Location
  , instantiationName :: Name
  , instantiationType :: Type
  }

declare :: Name -> Type -> Env -> Env
declare name type_ env = env
  { envTypes = Map.insert name type_ (envTypes env) }

emptyEnv :: Env
emptyEnv = Env
  { envClosure = []
  , envDefs = Map.empty
  , envLocals = []
  , envLocation = UnknownLocation
  , envNext = Name 0
  , envTypes = Map.empty
  }

findType :: Env -> Name -> Either CompileError Type
findType Env{..} name = maybeToEither nonexistent
  $ Map.lookup name envTypes
  where
  nonexistent :: CompileError
  nonexistent = TypeError envLocation $ unwords
    [ "nonexistent type variable:"
    , show name
    ]

freshName :: Env -> (Name, Env)
freshName env
  = let current = envNext env
  in (current, env { envNext = succ current })

freshVar :: Env -> (Type, Env)
freshVar env
  = let (name, env') = freshName env
  in (TypeVar name, env')

freshNameM :: Inferred Name
freshNameM = Inferred . lift $ state freshName

freshVarM :: Inferred Type
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
