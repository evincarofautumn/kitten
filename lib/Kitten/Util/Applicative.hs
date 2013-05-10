module Kitten.Util.Applicative
  ( (.&&.)
  , ffor
  ) where

import Control.Applicative

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

infixr 8 .&&.

ffor :: (Applicative f) => f a -> (a -> b) -> f b
ffor = flip fmap
