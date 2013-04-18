module Kitten.Util.List
  ( (!?)
  ) where

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? pred n
[] !? _ = Nothing
