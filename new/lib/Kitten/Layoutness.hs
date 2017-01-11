{-|
Module      : Kitten.Layoutness
Description : Whether a block is a layout block
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Layoutness
  ( Layoutness(..)
  ) where

-- | A display hint for whether a block was originally written with 'Layout'
-- (@:@) or 'Nonlayout' (@{}@) syntax.

data Layoutness = Layout | Nonlayout
  deriving (Show)
