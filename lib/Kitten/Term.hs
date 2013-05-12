{-# LANGUAGE RecordWildCards #-}

module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

import Kitten.Anno (Anno)
import Kitten.Builtin (Builtin)
import Kitten.Location

data Term
  = Push Value Location
  | Builtin Builtin Location
  | Lambda String [Term] Location
  | Block [Term]
  | If [Term] [Term] [Term] Location
  deriving (Eq, Show)

data Value
  = Bool Bool Location
  | Char Char Location
  | Escape String Location
  | Float Double Location
  | Function Anno [Term] Location
  | Int Int Location
  | Pair Value Value Location
  | Unit Location
  | Vector (Maybe Anno) [Value] Location
  | Word String Location
  deriving (Eq, Show)
