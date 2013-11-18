{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Location

data Anno = Anno Type Location
  deriving (Eq, Show)

data Type
  = Function !(Vector Type) !(Vector Type)
  | Bool
  | Char
  | Choice !Type !Type
  | Float
  | Handle
  | Int
  | Named !Text
  | Option !Type
  | Pair !Type !Type
  | RowFunction !Text !(Vector Type) !Text !(Vector Type)
  | Unit
  | Var !Text
  | Vector !Type
  deriving (Eq, Show)
