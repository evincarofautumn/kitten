module Util
  ( (<$$>)
  , maybeToEither
  ) where

import Control.Applicative

(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip fmap

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e
