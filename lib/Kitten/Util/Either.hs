module Kitten.Util.Either
  ( isLeft
  , isRight
  , mapLeft
  , mapRight
  ) where

isLeft :: Either a b -> Bool
isLeft Left{} = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight Right{} = True
isRight _ = False

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f = either (Left . f) Right

mapRight :: (a -> b) -> Either x a -> Either x b
mapRight f = either Left (Right . f)
