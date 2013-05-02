module Kitten.Util.Maybe
  ( justIf
  , applyMaybe
  , replaceMaybe
  ) where

import Control.Monad
import Control.Monad.Instances ()
import Data.Maybe

justIf :: Bool -> a -> Maybe a
justIf condition value = if condition then Just value else Nothing

applyMaybe :: (a -> Maybe a) -> a -> a
applyMaybe = ap fromMaybe

replaceMaybe :: (a -> Maybe a) -> [a] -> [a]
replaceMaybe f (x : xs) = applyMaybe f x : replaceMaybe f xs
replaceMaybe _ [] = []
