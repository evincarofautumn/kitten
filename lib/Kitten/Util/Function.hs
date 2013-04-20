module Kitten.Util.Function
  ( for
  ) where

for :: [a] -> (a -> b) -> [b]
for = flip map
