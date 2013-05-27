{-# LANGUAGE RecordWildCards #-}

module Kitten.Compile
  ( Config(..)
  , compile
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import System.IO

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Parse
import Kitten.Resolve
import Kitten.Resolved as Resolved
import Kitten.Scope
import Kitten.Tokenize
import Kitten.Typecheck

data Config = Config
  { dumpResolved :: Bool
  , dumpScoped :: Bool
  , name :: String
  , prelude :: [Def Resolved]
  , source :: String
  , stack :: [Resolved.Value]
  }

compile
  :: Config
  -> IO (Either CompileError (Fragment Resolved))
compile Config{..} = runEitherT $ do

  resolved <- hoistEither $ do
    tokenized <- liftParseError $ tokenize name source
    parsed <- liftParseError $ parse name tokenized
    resolve prelude parsed

  when dumpResolved . lift $ hPrint stderr resolved
  typechecked <- hoistEither $ typecheck prelude stack resolved

  let scoped = scope typechecked
  when dumpScoped . lift $ hPrint stderr scoped

  return scoped
