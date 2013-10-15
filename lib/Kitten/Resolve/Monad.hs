{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Resolve.Monad
  ( Env(..)
  , Resolution
  , compileError
  , defIndices
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
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Traversable (Traversable)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Def
import Kitten.Error
import Kitten.Typed (TypedDef)
import Kitten.Util.FailWriter (FailWriterT, runFailWriterT)

import qualified Kitten.Term as Term
import qualified Kitten.Util.FailWriter as FailWriter

newtype Resolution a = Resolution
  { unResolution :: FailWriterT [ErrorGroup] (State Env) a }
  deriving (Functor, Applicative, Monad)

data Env = Env
  { envPrelude :: !(Vector TypedDef)
  , envDefs :: !(Vector (Def Term.Value))
  , envScope :: [Text]
  }

-- | Halts resolution with a compilation error.
compileError :: ErrorGroup -> Resolution a
compileError err = Resolution $ FailWriter.throwMany [err]

defIndices :: Text -> Env -> Vector Int
defIndices expected Env{..} = findExpected envPrelude
  <> ((V.length envPrelude +) <$> findExpected envDefs)
  where
  findExpected :: Vector (Def a) -> Vector Int
  findExpected = V.findIndices $ (== expected) . defName

evalResolution :: Env -> Resolution a -> Either [ErrorGroup] a
evalResolution env (Resolution m)
  = evalState (runFailWriterT null m) env

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
