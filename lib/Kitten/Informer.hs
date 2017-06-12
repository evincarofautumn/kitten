{-|
Module      : Kitten.Informer
Description : Error-reporting monad
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Informer
  ( Informer(..)
  ) where

import Kitten.Origin (Origin)
import Kitten.Report (Report)
import qualified Text.PrettyPrint as Pretty

-- | Class of error-reporting monads.

class (Monad m) => Informer m where
  -- | Halt if there are any fatal reports.
  checkpoint :: m ()
  -- | Halt the computation.
  halt :: m a
  -- | Add a report to the log.
  report :: Report -> m ()
  -- | Add local context to reports.
  while :: Origin -> Pretty.Doc -> m a -> m a
