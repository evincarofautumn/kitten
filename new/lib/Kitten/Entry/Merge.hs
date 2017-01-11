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

-- | When adding a definition to the dictionary, if an existing definition has
-- the same name, the default 'Merge' behavior of 'Deny' raises an error, while
-- 'Compose' composes the bodies of the two definitions.

data Merge = Deny | Compose
  deriving (Eq, Show)
