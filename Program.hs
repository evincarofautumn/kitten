module Program
  ( Program(..)
  ) where

import Def

data Program a = Program [Def a] a

instance (Show a) => Show (Program a) where
  show (Program defs term) = unlines
    [ "Definitions:"
    , unlines $ map show defs
    , "Terms:"
    , show term
    ]
