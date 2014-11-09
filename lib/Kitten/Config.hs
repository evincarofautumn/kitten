{-# LANGUAGE DataKinds #-}

module Kitten.Config where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Definition
import Kitten.Kind
import Kitten.Term
import Kitten.Type

data Config = Config
  { configDumpResolved :: !Bool
  , configDumpScoped :: !Bool
  , configDumpTyped :: !Bool
  , configEnforceBottom :: !Bool
  , configFirstLine :: !Int
  , configImplicitPrelude :: !Bool
  , configLibraryDirectories :: [FilePath]
  , configName :: String
  , configOptimizations :: !OptConfig
  , configPredefined :: !(Vector (Def TypedTerm))
  , configSource :: !Text
  , configStackTypes :: Vector (Type Scalar)
  }

-- TODO Add flags for other optimizations.
data OptConfig = OptConfig
  { optUnusedDefElim :: !Bool
  }

defaultOptimizations :: OptConfig
defaultOptimizations = OptConfig
  { optUnusedDefElim = True
  }
