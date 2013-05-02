module Kitten.Util.Either
  ( mapLeft
  , mapRight
  ) where

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f = either (Left . f) Right

mapRight :: (a -> b) -> Either x a -> Either x b
mapRight f = either Left (Right . f)
