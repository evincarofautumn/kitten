module Kitten.Entry.Parent
  ( Parent(..)
  ) where

import Kitten.Name (Qualified)

data Parent
  = Trait !Qualified
  | Type !Qualified
  deriving (Show)
