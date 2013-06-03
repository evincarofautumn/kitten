module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Def

data Fragment a = Fragment
  { fragmentDefs :: [Def a]
  , fragmentTerms :: [a]
  } deriving (Eq, Show)
