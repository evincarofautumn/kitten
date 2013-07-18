module Kitten.Resolved
  ( Resolved(..)
  , Value(..)
  ) where

import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name

data Resolved
  = Builtin Builtin Location
  | Call Name Location
  | Compose [Resolved] Location
  | Group [Resolved] Location
  | If Resolved Resolved Location
  | PairTerm Resolved Resolved Location
  | Push Value Location
  | Scoped Resolved Location
  | VectorTerm [Resolved] Location
  deriving (Eq, Show)

data Value
  = Activation [Value] Resolved
  | Bool Bool
  | Char Char
  | Choice Bool Value
  | Closed Name
  | Closure [ClosedName] Resolved
  | Float Double
  | Function Resolved
  | Handle Handle
  | Int Int
  | Local Name
  | Option (Maybe Value)
  | Pair Value Value
  | Unit
  | Vector [Value]
  deriving (Eq, Show)
