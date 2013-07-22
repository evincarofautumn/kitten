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

import Kitten.Error
import Kitten.Fragment
import Kitten.Imports
import Kitten.Infer
import Kitten.Parse
import Kitten.Resolve
import Kitten.Scope
import Kitten.Tokenize
import Kitten.Resolved (Resolved, Value)
import Kitten.Util.Either
import Kitten.Util.Void

data Config = Config
  { dumpResolved :: Bool
  , dumpScoped :: Bool
  , name :: String
  , prelude :: Fragment Value Void
  , source :: String
  }

liftParseError :: Either ParseError a -> Either [CompileError] a
liftParseError = mapLeft ((:[]) . parseError)

compile
  :: Config
  -> IO (Either [CompileError] (Fragment Value Resolved))
compile Config{..} = liftM (mapLeft sort) . runEitherT $ do

  resolved <- hoistEither $ do
    tokenized <- liftParseError $ tokenize name source
    parsed <- liftParseError $ parse name tokenized
    substituted <- substituteImports parsed
    resolve prelude substituted

  when dumpResolved . lift $ hPrint stderr resolved
  hoistEither $ typeFragment prelude resolved

  let scoped = scope resolved
  when dumpScoped . lift $ hPrint stderr scoped

  return scoped
