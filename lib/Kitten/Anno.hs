{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Kitten.Location

data Anno = Anno Type Location
  deriving (Eq, Show)

data Type
  = Function [Type] [Type] Type
  | Bool
  | Char
  | Choice Type Type
  | Float
  | Handle
  | Int
  | Option Type
  | Pair Type Type
  | Unit
  | Var String
  | Vector Type
  | NoEffect
  | IOEffect
  | Join Type Type
  deriving (Eq, Show)
