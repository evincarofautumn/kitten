module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Def

data Fragment a = Fragment
  { fragmentDefs :: [Def a]
  , fragmentTerm :: a
  }
