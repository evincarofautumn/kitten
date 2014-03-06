module Kitten.Def
  ( Def(..)
  ) where

import Data.Text (Text)

import Kitten.Anno (Anno)
import Kitten.Location

data Def a = Def
  { defAnno :: !(Maybe Anno)
  , defLocation :: !Location
  , defName :: !Text
  , defTerm :: !a
  } deriving (Eq, Show)
