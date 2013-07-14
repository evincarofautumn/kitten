module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

import Kitten.Builtin (Builtin)
import Kitten.Location

data Term
  = Builtin Builtin Location
  | Call String Location
  | Compose [Term] Location
  | If Term Term Location
  | Lambda String Term Location
  | PairTerm Term Term Location
  | Push Value Location
  | VectorTerm [Term] Location
  deriving (Eq, Show)

data Value
  = Bool Bool Location
  | Char Char Location
  | Choice Bool Value Location
  | Float Double Location
  | Function [Term] Location
  | Int Int Location
  | Option (Maybe Value) Location
  | Pair Value Value Location
  | Unit Location
  | Vector [Value] Location
  deriving (Eq, Show)
