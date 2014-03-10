{-# LANGUAGE DataKinds #-}

module Kitten.Compile.Config
  ( Config(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Def
import Kitten.Tree
import Kitten.Type

import qualified Kitten.Infer.Config as Infer

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , firstLine :: !Int
  , implicitPrelude :: !Bool
  , inferConfig :: !Infer.Config
  , libraryDirectories :: [FilePath]
  , name :: String
  , predefined :: !(Vector (Def TypedTerm))
  , source :: !Text
  , stackTypes :: Vector (Type Scalar)
  }
