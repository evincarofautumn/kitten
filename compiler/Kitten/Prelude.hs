module Kitten.Prelude
  ( prelude
  ) where

import Data.Vector (Vector)

import qualified Data.Vector as Vector

import Kitten.Def
import Kitten.Resolve

prelude :: Vector (Def Resolved)
prelude = Vector.empty
