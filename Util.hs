module Util
  ( (<$$>)
  , maybeToEither
  , swap
  ) where

import Control.Applicative

(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip fmap

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)
