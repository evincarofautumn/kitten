module Kitten.Declaration
  ( Category(..)
  , Declaration(..)
  ) where

import Kitten.Name (Qualified)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)

data Category
  = Intrinsic
  | Trait
  deriving (Eq, Show)

data Declaration = Declaration
  { category :: !Category
  , name :: !Qualified
  , origin :: !Origin
  , signature :: !Signature
  } deriving (Show)
