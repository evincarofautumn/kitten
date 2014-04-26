{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Resolve.Monad
  ( Env(..)
  , Resolution
  , compileError
  , evalResolution
  , getsEnv
  , guardLiftM2
  , guardMapM
  , guardReturn
  , localIndex
  , modifyEnv
  , withLocal
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Text (Text)
import Data.Traversable (Traversable)

import Kitten.Def
import Kitten.Error
import Kitten.IR
import Kitten.Tree
import Kitten.Util.FailWriter (FailWriterT, runFailWriterT)

import qualified Kitten.Util.FailWriter as FailWriter

newtype Resolution a = Resolution
  { unResolution :: FailWriterT [ErrorGroup] (State Env) a }
  deriving (Functor, Applicative, Monad)

data Env = Env
  { envDefs :: !(HashMap Text (Def ParsedTerm))
  , envProgram :: !Program
  , envScope :: [Text]
  }

-- | Halts resolution with a compilation error.
compileError :: ErrorGroup -> Resolution a
compileError err = Resolution $ FailWriter.throwMany [err]

evalResolution :: Env -> Resolution a -> Either [ErrorGroup] a
evalResolution env (Resolution m) = evalState (runFailWriterT null m) env

getsEnv :: (Env -> a) -> Resolution a
getsEnv = Resolution . lift . gets

guardLiftM2 :: (a -> b -> c) -> Resolution a -> Resolution b -> Resolution c
guardLiftM2 f (Resolution a) (Resolution b)
  = Resolution $ FailWriter.guardLiftM2 f a b

guardMapM
  :: (Traversable t)
  => (a -> Resolution b) -> t a -> Resolution (t b)
guardMapM f xs
  = Resolution $ FailWriter.guardMapM (unResolution . f) xs

-- | Accumulates compile errors and resumes resolution.
guardReturn :: Resolution a -> Resolution (Maybe a)
guardReturn = Resolution . FailWriter.guardReturn . unResolution

localIndex :: Text -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

modifyEnv :: (Env -> Env) -> Resolution ()
modifyEnv = Resolution . lift . modify

withLocal :: Text -> Resolution a -> Resolution a
withLocal name action = do
  modifyEnv $ \env@Env{..} -> env { envScope = name : envScope }
  result <- action
  modifyEnv $ \env@Env{..} -> env { envScope = tail envScope }
  return result
