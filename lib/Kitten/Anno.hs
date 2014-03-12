{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Location

data Anno = Anno Type Location | TestAnno
  deriving (Show)

instance Eq Anno where
  TestAnno == _ = True
  _ == TestAnno = True
  Anno type1 loc1 == Anno type2 loc2 = (type1, loc1) == (type2, loc2)

data Type
  = Function !(Vector Type) !(Vector Type)
  | Bool
  | Char
  | Choice !Type !Type
  | Float
  | Handle
  | Int
  | Option !Type
  | Pair !Type !Type
  | RowFunction !Text !(Vector Type) !Text !(Vector Type)
  | Var !Text
  | Vector !Type
  deriving (Eq, Show)
