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

data Base = Binary | Octal | Decimal | Hexadecimal
  deriving (Show)
