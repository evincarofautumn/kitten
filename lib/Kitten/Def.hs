module Kitten.Def
  ( Def(..)
  ) where

import Kitten.Anno (Anno)
import Kitten.Location

data Def a = Def
  { defName :: String
  , defTerm :: a
  , defAnno :: Maybe Anno
  , defLocation :: Location
  } deriving (Eq, Show)
