{-|
Module      : Kitten.Entry.Merge
Description : Merge behavior for dictionary entries
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Entry.Merge
  ( Merge(..)
  ) where

data Merge = Deny | Compose
  deriving (Eq, Show)
