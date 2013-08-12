module Kitten.Import
  ( Import(..)
  ) where

import Data.Function
import Data.Text (Text)

import Kitten.Location

data Import = Import
  { importName :: Text
  , importLocation :: Location
  } deriving (Show)

instance Eq Import where
  (==) = (==) `on` importName
