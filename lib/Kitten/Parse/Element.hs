module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Data.Monoid

import Kitten.Anno (Anno)
import Kitten.Def
import Kitten.Term

data Element
  = DeclElement (Def Anno)
  | DefElement (Def Value)
  | TermElement Term

partitionElements
  :: [Element] -> ([Def Anno], [Def Value], [Term])
partitionElements = foldr go mempty
  where
  go element (decls, defs, terms) = case element of
    DeclElement decl -> (decl : decls, defs, terms)
    DefElement def -> (decls, def : defs, terms)
    TermElement term -> (decls, defs, term : terms)
