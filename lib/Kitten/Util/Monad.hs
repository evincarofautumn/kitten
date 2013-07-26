module Kitten.Util.Monad
  ( concatMapM
  , composeM
  ) where

import Control.Monad

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM = (liftM concat .) . mapM

composeM :: (Monad m) => [a -> m a] -> a -> m a
composeM = foldr (>=>) return
