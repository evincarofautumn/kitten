module Kitten.Entry.Category
  ( Category(..)
  ) where

data Category
  = Constructor
  | Instance
  | Permission
  | Word
  deriving (Eq, Show)
