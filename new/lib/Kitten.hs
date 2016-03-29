{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten
  ( Enter.fragmentFromSource
  , compile
  , runKitten
  , tokenize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Kitten.Dictionary (Dictionary)
import Kitten.Monad (K, runKitten)
import Kitten.Name
import Kitten.Tokenize (tokenize)
import Text.Parsec.Text ()
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.IO as IO

-- The compiler is a straightforward pipeline. At each stage, errors and
-- warnings ("reports") are accumulated, and reported to the programmer at the
-- next checkpoint.

compile
  :: [GeneralName]
  -> Maybe Qualified
  -> [FilePath]
  -> K Dictionary
compile mainPermissions mainName paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM IO.readFileUtf8 paths
  parsed <- mconcat <$> zipWithM
    (Enter.fragmentFromSource mainPermissions mainName 1)
    paths sources
  Enter.fragment parsed Dictionary.empty
