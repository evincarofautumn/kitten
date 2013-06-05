module Kitten.Util.Monoid
  ( mconcatMap
  ) where

import Data.Monoid

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap = (mconcat .) . map
