module Kitten.Util.Monad
  ( concatMapM
  ) where

import Control.Monad

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM = (liftM concat .) . mapM
