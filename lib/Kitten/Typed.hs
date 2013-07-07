module Kitten.Typed
  ( Typed(..)
  , Value(..)
  ) where

import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name

data Typed
  = Call Name Location
  | Compose [Typed]
  | Builtin Builtin Location
  | If Typed Typed Location
  | PairTerm Typed Typed Location
  | Push Value Location
  | Scoped Typed Location
  | VectorTerm [Typed] Location
  deriving (Eq, Show)

data Value
  = Activation [Value] Typed
  | Bool Bool
  | Char Char
  | Closed Name
  | Closure [ClosedName] Typed
  | Float Double
  | Function Typed
  | Handle Handle
  | Int Int
  | Local Name
  | Pair Value Value
  | Unit
  | Vector [Value]
  deriving (Eq, Show)
