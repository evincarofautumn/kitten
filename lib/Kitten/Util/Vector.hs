module Kitten.Util.Vector
  ( iforM
  , imapM
  , mapAndUnzipM
  , safeHead
  ) where

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector)

import qualified Data.Vector as V

imapM :: (Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f v = V.mapM (uncurry f) $ V.zipWith (,) (V.fromList [0 .. V.length v]) v

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = flip imapM

mapAndUnzipM
  :: (Monad m) => (a -> m (b, c)) -> Vector a -> m (Vector b, Vector c)
mapAndUnzipM = (liftM V.unzip .) . V.mapM

safeHead :: Vector a -> Maybe a
safeHead = listToMaybe . V.toList
