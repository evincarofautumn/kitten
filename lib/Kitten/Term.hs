module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

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
  | Function [Term] Location
  | Int Int Location
  | Local String Location
  | Pair Value Value Location
  | Unit Location
  | Vector [Value] Location
  deriving (Eq, Show)
