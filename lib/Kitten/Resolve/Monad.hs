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

import Kitten.Def
import Kitten.Error
import Kitten.Util.FailWriter (FailWriterT, runFailWriterT)

import qualified Kitten.Term as Term
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.FailWriter as FailWriter

newtype Resolution a = Resolution
  { unResolution :: FailWriterT [CompileError] (State Env) a }
  deriving (Functor, Applicative, Monad)

data Env = Env
  { envPrelude :: [Def Typed.Value]
  , envDefs :: [Def Term.Value]
  , envScope :: [String]
  }

-- | Halts resolution with a compilation error.
compileError :: CompileError -> Resolution a
compileError err = Resolution $ FailWriter.throwMany [err]

defIndices :: String -> Env -> [Int]
defIndices expected Env{..} = findExpected envPrelude
  ++ ((length envPrelude +) <$> findExpected envDefs)
  where findExpected = findIndices $ (== expected) . defName

evalResolution :: Env -> Resolution a -> Either [CompileError] a
evalResolution env (Resolution m)
  = evalState (runFailWriterT null m) env

getsEnv :: (Env -> a) -> Resolution a
getsEnv = Resolution . lift . gets

guardLiftM2 :: (a -> b -> c) -> Resolution a -> Resolution b -> Resolution c
guardLiftM2 f (Resolution a) (Resolution b)
  = Resolution $ FailWriter.guardLiftM2 f a b

guardMapM :: (a -> Resolution b) -> [a] -> Resolution [b]
guardMapM f xs
  = Resolution $ FailWriter.guardMapM (unResolution . f) xs

-- | Accumulates compile errors and resumes resolution.
guardReturn :: Resolution a -> Resolution (Maybe a)
guardReturn = Resolution . FailWriter.guardReturn . unResolution

localIndex :: String -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

modifyEnv :: (Env -> Env) -> Resolution ()
modifyEnv = Resolution . lift . modify

withLocal :: String -> Resolution a -> Resolution a
withLocal name action = do
  modifyEnv $ \ env@Env{..} -> env { envScope = name : envScope }
  result <- action
  modifyEnv $ \ env@Env{..} -> env { envScope = tail envScope }
  return result
