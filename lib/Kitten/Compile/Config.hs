{-# LANGUAGE DataKinds #-}

module Kitten.Compile.Config
  ( Config(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Types

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , firstLine :: !Int
  , implicitPrelude :: !Bool
  , libraryDirectories :: [FilePath]
  , name :: String
  , predefined :: !(Vector (Def TypedTerm))
  , source :: !Text
  , stackTypes :: Vector (Type Scalar)
  }
