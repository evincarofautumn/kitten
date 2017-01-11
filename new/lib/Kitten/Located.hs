{-|
Module      : Kitten.Located
Description : Imbuing a value with a location
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Located
  ( Located(..)
  ) where

import Kitten.Indent (Indent)
import Kitten.Origin (Origin)

-- | Imbues a value (such as a 'Token') with an origin and indent level.

data Located a = At
  { origin :: !Origin
  , indent :: !Indent
  , item :: a
  }

instance (Show a) => Show (Located a) where
  show = show . item
