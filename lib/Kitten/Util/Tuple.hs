module Kitten.Util.Tuple
  ( swap
  ) where

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)
