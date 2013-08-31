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
  = Function !(Vector Type) !(Vector Type) !Type
  | Bool
  | Char
  | Choice !Type !Type
  | Float
  | Handle
  | Int
  | Named !Text
  | Option !Type
  | Pair !Type !Type
  | Unit
  | Var !Text
  | Vector !Type
  | NoEffect
  | IOEffect
  | Join !Type !Type
  deriving (Eq, Show)
