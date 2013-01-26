module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Vector (Vector)

import Kitten.Def

data Fragment a = Fragment
  { fragmentDefs :: Vector (Def a)
  , fragmentTerm :: a
  }
