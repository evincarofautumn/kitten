module Kitten.Util.Set
  ( Kitten.Util.Set.mapM
  ) where

import Control.Monad
import Data.Set (Set)

import qualified Data.Set as Set

mapM :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapM f = liftM Set.fromList . Prelude.mapM f . Set.toList
