module Kitten.Util
  ( (<$$>)
  , mapLeft
  , maybeToEither
  , swap
  , textShow
  ) where

import Data.Text (Text)

import qualified Data.Text as Text

(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

textShow :: (Show a) => a -> Text
textShow = Text.pack . show
