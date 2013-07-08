module Kitten.Purity
  ( Purity(..)
  , (<=>)
  ) where

data Purity = Pure | Impure
  deriving (Eq)

(<=>) :: Purity -> Purity -> Purity
Impure <=> _ = Impure
_ <=> Impure = Impure
_ <=> _ = Pure

instance Show Purity where
  show purity = case purity of
    Pure -> " -> "
    Impure -> " => "
