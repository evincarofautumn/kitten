{-|
Module      : Kitten.Entry.Category
Description : Types of dictionary entries
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Entry.Category
  ( Category(..)
  ) where

data Category
  = Constructor
  | Instance
  | Permission
  | Word
  deriving (Eq, Show)
