module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Vector (Vector)

import Kitten.Anno (Anno)
import Kitten.Def

data Fragment a b = Fragment
  { fragmentAnnos :: !(Vector (Anno b))
  , fragmentDefs :: !(Vector (Def a))
  , fragmentTerm :: !a
  }
