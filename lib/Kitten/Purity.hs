module Kitten.Purity
  ( Purity(..)
  , (<=>)
  ) where

data Purity = Pure | Impure
  deriving (Eq, Show)

(<=>) :: Purity -> Purity -> Purity
Impure <=> _ = Impure
_ <=> Impure = Impure
_ <=> _ = Pure
