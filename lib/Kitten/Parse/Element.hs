module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Kitten.Def
import Kitten.Term

data Element
  = DefElement (Def Term)
  | TermElement Term

partitionElements :: [Element] -> ([Def Term], [Term])
partitionElements = foldr partitionElement ([], [])
  where
  partitionElement (DefElement d) (ds, ts) = (d : ds, ts)
  partitionElement (TermElement t) (ds, ts) = (ds, t : ts)
