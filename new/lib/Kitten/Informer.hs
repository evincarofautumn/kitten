module Kitten.Informer
  ( Informer(..)
  ) where

import Kitten.Origin (Origin)
import Kitten.Report (Report)
import qualified Text.PrettyPrint as Pretty

class (Monad m) => Informer m where
  checkpoint :: m ()
  halt :: m a
  report :: Report -> m ()
  while :: [Pretty.Doc] -> Origin -> m a -> m a
