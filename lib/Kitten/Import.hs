module Kitten.Import where

import Data.Function

import Kitten.Location
import Kitten.Name

data Import = Import
  { importName :: !Name
  , importLocation :: !Location
  } deriving (Show)

instance Eq Import where
  (==) = (==) `on` importName
