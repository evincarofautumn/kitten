module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Data.Monoid

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Term

data Element
  = DefElement (Def Value)
  | ImportElement Import
  | TermElement Term

partitionElements :: [Element] -> Fragment Value Term
partitionElements = foldr go mempty
  where
  go element acc = case element of
    DefElement def -> acc
      { fragmentDefs = def : fragmentDefs acc }
    ImportElement import_ -> acc
      { fragmentImports = import_ : fragmentImports acc }
    TermElement term -> acc
      { fragmentTerms = term : fragmentTerms acc }
