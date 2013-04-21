module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Anno
import Kitten.Def

data Fragment a = Fragment
  { fragmentAnnos :: [Anno]
  , fragmentDefs :: [Def a]
  , fragmentTerms :: [a]
  } deriving (Eq, Show)
