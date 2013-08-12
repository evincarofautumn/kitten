module Kitten.Def
  ( Def(..)
  ) where

import Data.Text (Text)

import Kitten.Anno (Anno)
import Kitten.Location

data Def a = Def
  { defName :: Text
  , defTerm :: a
  , defAnno :: Maybe Anno
  , defLocation :: Location
  } deriving (Eq, Show)
