module Kitten.Trait
  ( Trait(..)
  ) where

import Kitten.Name (Qualified)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)

data Trait = Trait
  { name :: !Qualified
  , origin :: !Origin
  , signature :: !Signature
  } deriving (Show)
