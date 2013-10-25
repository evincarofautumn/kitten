{-# LANGUAGE DataKinds #-}

module Kitten.Compile.Config
  ( Config(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Fragment
import Kitten.Type
import Kitten.Typed (Typed)

import qualified Kitten.Infer.Config as Infer

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , firstLine :: !Int
  , inferConfig :: !Infer.Config
  , libraryDirectories :: [FilePath]
  , name :: String
  , prelude :: !(Fragment Typed)
  , source :: !Text
  , stackTypes :: Vector (Type Scalar)
  }
