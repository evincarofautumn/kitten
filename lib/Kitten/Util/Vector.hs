module Kitten.Util.Vector
  ( iforM
  , imapM
  , mapAndUnzipM
  ) where

import Control.Monad (liftM, zipWithM)
import Data.Vector (Vector)

import qualified Data.Vector as V

imapM :: (Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f = liftM V.fromList . zipWithM f [0..] . V.toList

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = flip imapM

mapAndUnzipM
  :: (Monad m) => (a -> m (b, c)) -> Vector a -> m (Vector b, Vector c)
mapAndUnzipM = (liftM V.unzip .) . V.mapM
