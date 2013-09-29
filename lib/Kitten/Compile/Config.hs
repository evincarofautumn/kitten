module Kitten.Compile.Config
  ( Config(..)
  ) where

import Data.Text (Text)

import Kitten.Fragment
import Kitten.Resolved

import qualified Kitten.Infer.Config as Infer

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , inferConfig :: !Infer.Config
  , libraryDirectories :: [FilePath]
  , name :: String
  , prelude :: !(Fragment Resolved)
  , source :: !Text
  , stack :: [Value]
  }
