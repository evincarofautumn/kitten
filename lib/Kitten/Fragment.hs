module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Anno (Anno)
import Kitten.Def

data Fragment a b = Fragment
  { fragmentDecls :: [Def Anno]
  , fragmentDefs :: [Def a]
  , fragmentTerms :: [b]
  } deriving (Eq, Show)
