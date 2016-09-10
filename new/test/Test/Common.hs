module Test.Common
  ( Sign(..)
  ) where

data Sign = Negative | Positive
  deriving (Eq, Ord, Show)
