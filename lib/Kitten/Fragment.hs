module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Anno
import Kitten.Def

data Fragment a = Fragment
  { fragmentAnnos :: [Anno]
  , fragmentDefs :: [Def a]
  , fragmentTerm :: a
  } deriving (Eq, Show)
