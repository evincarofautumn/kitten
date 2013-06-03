module Kitten.Def
  ( Def(..)
  ) where

import Kitten.Location

data Def a = Def
  { defName :: String
  , defTerm :: a
  , defLocation :: Location
  } deriving (Eq)

instance (Show a) => Show (Def a) where
  show (Def name body _) = unwords ["def", name, show body]
