module Kitten.Located
  ( Located(..)
  ) where

import Kitten.Indent (Indent)
import Kitten.Origin (Origin)

data Located a = At
  { origin :: !Origin
  , indent :: !Indent
  , item :: a
  } deriving (Show)
