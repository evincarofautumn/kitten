module Fragment
  ( Fragment(..)
  ) where

import Def

data Fragment a = Fragment
  { fragmentDefs :: [Def a]
  , fragmentTerm :: a
  }
