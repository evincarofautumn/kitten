module Kitten.Util.Tuple
  ( both
  , swap
  ) where

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)
