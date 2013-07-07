module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Data.Monoid

import Kitten.Def
import Kitten.Term

data Element
  = DefElement (Def Value)
  | TermElement Term

partitionElements
  :: [Element] -> ([Def Value], [Term])
partitionElements = foldr go mempty
  where
  go element (defs, terms) = case element of
    DefElement def -> (def : defs, terms)
    TermElement term -> (defs, term : terms)
