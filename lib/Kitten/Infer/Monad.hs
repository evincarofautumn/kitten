{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Vector (Vector)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.NameMap (NameMap)
import Kitten.Type
import Kitten.Util.FailWriter
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

import qualified Kitten.NameMap as N

data Env = Env
  { envClosure :: !(Vector (Type Scalar))
  , envDefs :: !(NameMap Scheme)
  , envEffects :: !(NameMap (Type Effect))
  , envLocals :: [Type Scalar]
  , envLocation :: !Location
  , envNext  :: !Name
  , envRows :: !(NameMap (Type Row))
  , envScalars :: !(NameMap (Type Scalar))
  , envTypeDefs :: !(Map Text Scheme)
  }

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [CompileError] (State Env) a
  } deriving (Applicative, Functor, Monad)

class Declare a where
  declare :: TypeName a -> Type a -> Env -> Env

instance Declare Effect where
  declare (TypeName name) type_ env = env
    { envEffects = N.insert name type_ (envEffects env) }

instance Declare Row where
  declare (TypeName name) type_ env = env
    { envRows = N.insert name type_ (envRows env) }

instance Declare Scalar where
  declare (TypeName name) type_ env = env
    { envScalars = N.insert name type_ (envScalars env) }

emptyEnv :: Env
emptyEnv = Env
  { envClosure = V.empty
  , envDefs = N.empty
  , envEffects = N.empty
  , envLocals = []
  , envLocation = UnknownLocation
  , envNext = Name 0
  , envRows = N.empty
  , envScalars = N.empty
  , envTypeDefs = M.empty
  }

class Retrieve a where
  retrieve :: Env -> TypeName a -> Either CompileError (Type a)

instance Retrieve Effect where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envEffects)
    $ TypeError envLocation $ T.unwords
    [ "nonexistent effect variable:"
    , toText name
    ]

instance Retrieve Row where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envRows)
    $ TypeError envLocation $ T.unwords
    [ "nonexistent row type variable:"
    , toText name
    ]

instance Retrieve Scalar where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envScalars)
    $ TypeError envLocation $ T.unwords
    [ "nonexistent scalar type variable:"
    , toText name
    ]

freshName :: Env -> (Name, Env)
freshName env
  = let current = envNext env
  in (current, env { envNext = succ current })

freshVar :: Location -> Env -> (Type a, Env)
freshVar loc env
  = let (name, env') = freshName env
  in (Var name loc, env')

freshNameM :: Inferred Name
freshNameM = Inferred . lift $ state freshName

freshVarM :: Inferred (Type a)
freshVarM = do
  loc <- getsEnv envLocation
  Inferred . lift $ state (freshVar loc)

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
