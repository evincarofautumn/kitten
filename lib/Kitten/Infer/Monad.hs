{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Config(..)
  , Env(..)
  , Inferred
  , Declare(..)
  , asksConfig
  , emptyEnv
  , freshNameM
  , freshVar
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
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Text (Text)

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
  , envDefs :: !(NameMap Scheme)
  , envDecls :: !(NameMap Scheme)
  , envEffects :: !(NameMap (Type Effect))
  , envLocals :: [Type Scalar]
  , envLocation :: !Location
  , envNameGen :: !NameGen
  , envRows :: !(NameMap (Type Row))
  , envScalars :: !(NameMap (Type Scalar))
  , envTypeDefs :: !(Map Text Scheme)
  }

newtype Inferred a = Inferred
  { unwrapInferred :: FailWriterT [ErrorGroup] (ReaderT Config (State Env)) a
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
  , envDecls = N.empty
  , envEffects = N.empty
  , envLocals = []
  , envLocation = UnknownLocation
  , envNameGen = mkNameGen
  , envRows = N.empty
  , envScalars = N.empty
  , envTypeDefs = M.empty
  }

class Retrieve a where
  retrieve :: Env -> TypeName a -> Either ErrorGroup (Type a)

instance Retrieve Effect where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envEffects)
    $ nonexistent "effect" envLocation name

instance Retrieve Row where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envRows)
    $ nonexistent "row" envLocation name

instance Retrieve Scalar where
  retrieve Env{..} (TypeName name)
    = flip maybeToEither (N.lookup name envScalars)
    $ nonexistent "scalar" envLocation name

nonexistent :: Text -> Location -> Name -> ErrorGroup
nonexistent kind loc name
  = oneError $ CompileError loc Error $ T.concat
  [ "nonexistent ", kind, " variable '"
  , toText name
  , "'"
  ]

asksConfig :: (Config -> a) -> Inferred a
asksConfig = Inferred . lift . asks

freshName :: Env -> (Name, Env)
freshName env
  = let (name, gen') = genName (envNameGen env)
  in (name, env { envNameGen = gen' })

freshVar :: Location -> Env -> (Type a, Env)
freshVar loc env
  = let (name, env') = freshName env
  in (Var name loc, env')

freshNameM :: Inferred Name
freshNameM = liftState $ state freshName

freshVarM :: Inferred (Type a)
freshVarM = do
  loc <- getsEnv envLocation
  liftState $ state (freshVar loc)

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
withLocation here action = do
  there <- getsEnv envLocation
  modifyEnv $ \env -> env { envLocation = here }
  result <- action
  modifyEnv $ \env -> env { envLocation = there }
  return result
