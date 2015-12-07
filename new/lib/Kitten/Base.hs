module Kitten.Base
  ( Base(..)
  ) where

data Base = Binary | Octal | Decimal | Hexadecimal
  deriving (Show)
