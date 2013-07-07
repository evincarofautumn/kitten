module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Def

data Fragment a b = Fragment
  { fragmentDefs :: [Def a]
  , fragmentTerms :: [b]
  } deriving (Eq, Show)
