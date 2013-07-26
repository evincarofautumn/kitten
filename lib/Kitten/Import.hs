module Kitten.Import
  ( Import(..)
  ) where

import Data.Function

import Kitten.Location

data Import = Import
  { importName :: String
  , importLocation :: Location
  } deriving (Show)

instance Eq Import where
  (==) = (==) `on` importName
