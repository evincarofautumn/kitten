module Kitten.TypeDef
  ( TypeDef(..)
  ) where

import Kitten.Anno (Anno)
import Kitten.Location

data TypeDef = TypeDef
  { typeDefName :: String
  , typeDefAnno :: Anno
  , typeDefLocation :: Location
  } deriving (Eq, Show)
