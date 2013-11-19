{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Config(..)
  , Env(..)
  , Inferred
  , Declare(..)
  , asksConfig
  , emptyEnv
  , envLocation
  , freshNameM
  , freshConst
  , freshVar
  , freshConstM
  , freshVarM
  , getEnv
  , getsEnv
  , liftFailWriter
  , liftState
  , modifyEnv
  , putEnv
  , retrieve
  , runInference
  , withLocation
  , withOrigin
  ) where

import Control.Applicative hiding (Const)
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Infer.Config
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
  , envDefs :: !(NameMap TypeScheme)
  , envDecls :: !(NameMap TypeScheme)
  , envLocals :: [Type Scalar]
  , envNameGen :: !NameGen
  , envOrigin :: !Origin
  , envRows :: !(NameMap (Type Row))
  , envScalars :: !(NameMap (Type Scalar))
  , envTypeDefs :: !(Map Text TypeScheme)
  }

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [ErrorGroup] (ReaderT Config (State Env)) a
  } deriving (Applicative, Functor, Monad, MonadFix)

class Declare a where
  declare :: TypeName a -> Type a -> Env -> Env

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
  , envDecls = N.empty
  , envLocals = []
  , envNameGen = mkNameGen
  , envOrigin = Origin NoHint UnknownLocation
  , envRows = N.empty
  , envScalars = N.empty
  , envTypeDefs = M.empty
  }

class Retrieve a where
  retrieve :: Env -> TypeName a -> Either ErrorGroup (Type a)

instance Retrieve Row where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envRows)
    $ nonexistent "row" envOrigin name

instance Retrieve Scalar where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envScalars)
    $ nonexistent "scalar" envOrigin name

nonexistent :: Text -> Origin -> Name -> ErrorGroup
nonexistent kind (Origin _ loc) name
  = oneError $ CompileError loc Error $ T.concat
  [ "nonexistent ", kind, " variable '"
  , toText name
  , "'"
  ]

asksConfig :: (Config -> a) -> Inferred a
asksConfig = Inferred . lift . asks

envLocation :: Env -> Location
envLocation env = let Origin _ loc = envOrigin env in loc

freshName :: Env -> (TypeName a, Env)
freshName env
  = let (name, gen') = genName (envNameGen env)
  in (TypeName name, env { envNameGen = gen' })

fresh
  :: forall a (b :: Kind -> *). (TypeName a -> Origin -> b a)
  -> Origin -> Env -> (b a, Env)
fresh constructor origin env
  = let (typeName, env') = freshName env
  in (constructor typeName origin, env')

freshConst :: Origin -> Env -> (Type Scalar, Env)
freshConst = fresh Const

freshVar :: Origin -> Env -> (Type a, Env)
freshVar = fresh Var

freshNameM :: Inferred (TypeName a)
freshNameM = liftState $ state freshName

freshM
  :: forall a (b :: Kind -> *). (Origin -> Env -> (b a, Env))
  -> Inferred (b a)
freshM action = do
  Origin hint loc <- getsEnv envOrigin
  liftState $ state (action (Origin hint loc))

freshConstM :: Inferred (Type Scalar)
freshConstM = freshM freshConst

freshVarM :: forall (a :: Kind). Inferred (Type a)
freshVarM = freshM freshVar

getEnv :: Inferred Env
getEnv = liftState get

getsEnv :: (Env -> a) -> Inferred a
getsEnv = liftState . gets

liftFailWriter
  :: FailWriterT [ErrorGroup] (ReaderT Config (State Env)) a
  -> Inferred a
liftFailWriter = Inferred

liftState :: State Env a -> Inferred a
liftState = Inferred . lift . lift

modifyEnv :: (Env -> Env) -> Inferred ()
modifyEnv = liftState . modify

putEnv :: Env -> Inferred ()
putEnv = liftState . put

runInference
  :: Config
  -> Env
  -> Inferred a
  -> (Either [ErrorGroup] a, Env)
runInference config env action
  = flip runState env
  . flip runReaderT config
  . runFailWriterT null
  $ unwrapInferred action

withLocation :: Location -> Inferred a -> Inferred a
withLocation here = withOrigin (Origin NoHint here)

withOrigin :: Origin -> Inferred a -> Inferred a
withOrigin here action = do
  there <- getsEnv envOrigin
  modifyEnv $ \env -> env { envOrigin = here }
  result <- action
  modifyEnv $ \env -> env { envOrigin = there }
  return result
