module Kitten.Import
  ( Import(..)
  ) where

import Kitten.Location

data Import = Import
  { importName :: String
  , importLocation :: Location
  } deriving (Eq, Show)
