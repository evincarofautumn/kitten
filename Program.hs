module Program
  ( Program(..)
  ) where

import Def

data Program a = Program
  { programDefs :: [Def a] 
  , programTerm :: a
  }
