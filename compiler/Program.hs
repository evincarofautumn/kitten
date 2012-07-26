module Program
  ( Program(..)
  ) where

import Term

data Program = Program [Term]
