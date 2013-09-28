module Kitten.Util.Monad
  ( concatMapM
  , composeM
  , whenJust
  ) where

import Control.Monad
import Data.Traversable (Traversable)

import qualified Data.Traversable as T

concatMapM
  :: (Monad m, Traversable t, Monad t)
  => (a -> m (t b)) -> t a -> m (t b)
concatMapM = (liftM join .) . T.mapM

composeM :: (Monad m) => [a -> m a] -> a -> m a
composeM = foldr (>=>) return

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust (Just x) m = m x >> return ()
whenJust Nothing _ = return ()
