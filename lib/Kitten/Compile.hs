{-# LANGUAGE RecordWildCards #-}

module Kitten.Compile
  ( Config(..)
  , compile
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.List (sort)
import System.IO
import Text.Parsec.Error

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Infer
import Kitten.Parse
import Kitten.Resolve
import Kitten.Scope
import Kitten.Tokenize
import Kitten.Typed (Typed)
import Kitten.Util.Either

import qualified Kitten.Typed as Typed

data Config = Config
  { dumpResolved :: Bool
  , dumpScoped :: Bool
  , name :: String
  , prelude :: [Def Typed.Value]
  , source :: String
  }

liftParseError :: Either ParseError a -> Either [CompileError] a
liftParseError = mapLeft ((:[]) . parseError)

compile
  :: Config
  -> IO (Either [CompileError] (Fragment Typed.Value Typed))
compile Config{..} = liftM (mapLeft sort) . runEitherT $ do

  resolved <- hoistEither $ do
    tokenized <- liftParseError $ tokenize name source
    parsed <- liftParseError $ parse name tokenized
    resolve prelude parsed

  when dumpResolved . lift $ hPrint stderr resolved
  inferred <- hoistEither $ typeFragment prelude resolved

  let scoped = scope inferred
  when dumpScoped . lift $ hPrint stderr scoped

  return scoped
