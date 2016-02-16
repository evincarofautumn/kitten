module Kitten.Located
  ( Located(..)
  ) where

import Kitten.Indent (Indent)
import Kitten.Origin (Origin)

data Located a = At
  { origin :: !Origin
  , indent :: !Indent
  , item :: a
  }

instance (Show a) => Show (Located a) where
  show = show . item
