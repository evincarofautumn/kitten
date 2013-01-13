module Program
  ( Program(..)
  ) where

import Def

data Program a = Program [Def a] a
