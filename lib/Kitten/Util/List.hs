module Kitten.Util.List
  ( (!?)
  , findMap
  , for
  , list
  , mapHead
  , small
  , stripCommonPrefix
  ) where

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? pred n
[] !? _ = Nothing

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = go
  where
  go (x : xs) = case f x of
    y@Just{} -> y
    Nothing -> go xs
  go [] = Nothing

for :: [a] -> (a -> b) -> [b]
for = flip map

list :: a -> [a]
list = (:[])

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x : xs) = f x : xs
mapHead _ [] = []

small :: [a] -> Bool
small [] = True
small [_] = True
small _ = False

stripCommonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
stripCommonPrefix (x : xs) (y : ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix xs ys = (xs, ys)
