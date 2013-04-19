module Kitten.Util.List
  ( (!?)
  , stripCommonPrefix
  ) where

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? pred n
[] !? _ = Nothing

stripCommonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
stripCommonPrefix (x : xs) (y : ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix xs ys = (xs, ys)
