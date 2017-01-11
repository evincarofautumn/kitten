{-|
Module      : Kitten
Description : Compiler pipeline
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten
  ( Enter.fragmentFromSource
  , compile
  , runKitten
  , tokenize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Kitten.CollectInstantiations (collectInstantiations)
import Kitten.Dictionary (Dictionary)
import Kitten.Monad (K, runKitten)
import Kitten.Name
import Kitten.Tokenize (tokenize)
import Text.Parsec.Text ()
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.IO as IO

-- | This is a simple wrapper for the compiler pipeline. It adds a list of
-- program fragments to the dictionary from a list of source paths. At each
-- stage, errors and warnings (\"reports\") are accumulated, and reported to the
-- programmer at the next checkpoint; see "Kitten.Monad" for details.

compile
  :: [GeneralName]
  -- ^ List of permissions to grant to @main@.
  -> Maybe Qualified
  -- ^ Override the default name of @main@.
  -> [FilePath]
  -- ^ List of source file paths.
  -> K Dictionary
  -- ^ Resulting dictionary.
compile mainPermissions mainName paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM IO.readFileUtf8 paths
  parsed <- mconcat <$> zipWithM
    (Enter.fragmentFromSource mainPermissions mainName 1)
    paths sources
  -- dictionary <-
  Enter.fragment parsed Dictionary.empty
  -- collectInstantiations dictionary
