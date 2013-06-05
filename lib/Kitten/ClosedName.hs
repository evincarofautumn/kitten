module Kitten.ClosedName
  ( ClosedName(..)
  ) where

import Kitten.Name

data ClosedName
  = ClosedName Name
  | ReclosedName Name
  deriving (Eq, Show)
