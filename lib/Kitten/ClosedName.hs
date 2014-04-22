module Kitten.ClosedName
  ( ClosedName(..)
  ) where

data ClosedName
  = ClosedName !Int
  | ReclosedName !Int
  deriving (Eq, Show)
