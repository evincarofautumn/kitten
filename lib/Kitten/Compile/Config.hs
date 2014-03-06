{-# LANGUAGE DataKinds #-}

module Kitten.Compile.Config
  ( Config(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Fragment
import Kitten.Tree
import Kitten.Type

import qualified Kitten.Infer.Config as Infer

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , firstLine :: !Int
  , inferConfig :: !Infer.Config
  , libraryDirectories :: [FilePath]
  , name :: String
  , prelude :: !(Fragment TypedTerm)
  , source :: !Text
  , stackTypes :: Vector (Type Scalar)
  }
