module Kitten.Util.Function
  ( (..:)
  , compose
  ) where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(..:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(..:) = (.:) . (.)

compose :: [a -> a] -> a -> a
compose = foldr (.) id
