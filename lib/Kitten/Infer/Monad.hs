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
  , freshConst
  , freshIdM
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
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Infer.Config
import Kitten.Location
import Kitten.Id
import Kitten.IdMap (IdMap)
import Kitten.Type
import Kitten.Util.FailWriter
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

import qualified Kitten.IdMap as Id

data Env = Env
  { envClosure :: !(Vector (Type Scalar))
  , envDefs :: !(HashMap Text TypeScheme)
  , envDecls :: !(HashMap Text TypeScheme)
  , envIdGen :: !IdGen
  , envLocals :: [Type Scalar]
  , envOrigin :: !Origin
  , envScalars :: !(IdMap (Type Scalar))
  , envStacks :: !(IdMap (Type Stack))
  , envTypeDefs :: !(Map Text TypeScheme)
  }

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [ErrorGroup] (ReaderT Config (State Env)) a
  } deriving (Applicative, Functor, Monad, MonadFix)

class Declare a where
  declare :: TypeId a -> Type a -> Env -> Env

instance Declare Stack where
  declare (TypeId i) type_ env = env
    { envStacks = Id.insert i type_ (envStacks env) }

instance Declare Scalar where
  declare (TypeId i) type_ env = env
    { envScalars = Id.insert i type_ (envScalars env) }

emptyEnv :: Location -> Env
emptyEnv loc = Env
  { envClosure = V.empty
  , envDefs = H.empty
  , envDecls = H.empty
  , envIdGen = mkIdGen
  , envLocals = []
  , envOrigin = Origin NoHint loc
  , envScalars = Id.empty
  , envStacks = Id.empty
  , envTypeDefs = M.empty
  }

class Retrieve a where
  retrieve :: Env -> TypeId a -> Either ErrorGroup (Type a)

instance Retrieve Stack where
  retrieve Env{..} (TypeId name)
    = flip maybeToEither (Id.lookup name envStacks)
    $ nonexistent "stack" envOrigin name

instance Retrieve Scalar where
  retrieve Env{..} (TypeId name)
    = flip maybeToEither (Id.lookup name envScalars)
    $ nonexistent "scalar" envOrigin name

nonexistent :: Text -> Origin -> Id -> ErrorGroup
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

freshId :: Env -> (TypeId a, Env)
freshId env
  = let (name, gen') = genId (envIdGen env)
  in (TypeId name, env { envIdGen = gen' })

fresh
  :: forall a (b :: Kind -> *). (TypeId a -> Origin -> b a)
  -> Origin -> Env -> (b a, Env)
fresh constructor origin env
  = let (typeId, env') = freshId env
  in (constructor typeId origin, env')

freshConst :: Origin -> Env -> (Type a, Env)
freshConst = fresh Const

freshVar :: Origin -> Env -> (Type a, Env)
freshVar = fresh Var

freshIdM :: Inferred (TypeId a)
freshIdM = liftState $ state freshId

freshM
  :: forall a (b :: Kind -> *). (Origin -> Env -> (b a, Env))
  -> Inferred (b a)
freshM action = do
  Origin hint loc <- getsEnv envOrigin
  liftState $ state (action (Origin hint loc))

freshConstM :: Inferred (Type a)
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
