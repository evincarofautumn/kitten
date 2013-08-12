module Kitten.TypeDef
  ( TypeDef(..)
  ) where

import Data.Text (Text)

import Kitten.Anno (Anno)
import Kitten.Location

data TypeDef = TypeDef
  { typeDefName :: Text
  , typeDefAnno :: Anno
  , typeDefLocation :: Location
  } deriving (Eq, Show)
