module Kitten.Util.Vector
  ( iforM
  , mapAndUnzipM
  , safeHead
  ) where

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector)

import qualified Data.Vector as V

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = flip V.imapM

mapAndUnzipM
  :: (Monad m) => (a -> m (b, c)) -> Vector a -> m (Vector b, Vector c)
mapAndUnzipM = (liftM V.unzip .) . V.mapM

safeHead :: Vector a -> Maybe a
safeHead = listToMaybe . V.toList
