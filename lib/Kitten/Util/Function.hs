module Kitten.Util.Function
  ( compose
  ) where

compose :: [a -> a] -> a -> a
compose = foldr (.) id
