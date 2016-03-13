module Kitten.Entry.Merge
  ( Merge(..)
  ) where

data Merge = Deny | Compose
  deriving (Eq, Show)
