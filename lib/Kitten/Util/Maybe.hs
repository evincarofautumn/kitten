module Kitten.Util.Maybe
  ( applyMaybe
  , justIf
  , maybeToEither
  , replaceMaybe
  ) where

import Control.Monad
import Control.Monad.Instances ()
import Data.Maybe

applyMaybe :: (a -> Maybe a) -> a -> a
applyMaybe = ap fromMaybe

justIf :: Bool -> a -> Maybe a
justIf condition value = if condition then Just value else Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

replaceMaybe :: (a -> Maybe a) -> [a] -> [a]
replaceMaybe f (x : xs) = applyMaybe f x : replaceMaybe f xs
replaceMaybe _ [] = []
