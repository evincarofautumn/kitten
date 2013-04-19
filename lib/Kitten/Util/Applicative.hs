module Kitten.Util.Applicative
  ( (.&&.)
  ) where

import Control.Applicative

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

infixr 8 .&&.
