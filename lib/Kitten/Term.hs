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
  | PairTerm [Term] [Term] Location
  | Push Value Location
  | VectorTerm [[Term]] Location
  deriving (Eq, Show)

data Value
  = Bool Bool Location
  | Char Char Location
  | Float Double Location
  | Function [Term] Location
  | Int Int Location
  | Pair Value Value Location
  | Unit Location
  | Vector [Value] Location
  deriving (Eq, Show)
