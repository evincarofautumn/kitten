{-|
Module      : Kitten.Base
Description : Numeric literal bases
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Base
  ( Base(..)
  ) where

-- | The radix of an integer literal.

data Base
  -- | @0b@
  = Binary
  -- | @0o@
  | Octal
  -- | No prefix.
  | Decimal
  -- | @0x@
  | Hexadecimal
  deriving (Show)
