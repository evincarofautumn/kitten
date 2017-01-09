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

class (Monad m) => Informer m where
  checkpoint :: m ()
  halt :: m a
  report :: Report -> m ()
  while :: Origin -> Pretty.Doc -> m a -> m a
