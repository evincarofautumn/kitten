module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

import Kitten.Anno (Anno)
import Kitten.Builtin (Builtin)
import Kitten.Location

data Term
  = Block [Term]
  | Builtin Builtin Location
  | Call String Location
  | If [Term] [Term] [Term] Location
  | Lambda String [Term] Location
  | Push Value Location
  deriving (Eq, Show)

data Value
  = Bool Bool Location
  | Char Char Location
  | Escape String Location
  | Float Double Location
  | Function (Maybe Anno) [Term] Location
  | Int Int Location
  | Pair Value Value Location
  | Unit Location
  | Vector (Maybe Anno) [Value] Location
  deriving (Eq, Show)
