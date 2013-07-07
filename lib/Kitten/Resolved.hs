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
  = Block [Resolved]
  | Builtin Builtin Location
  | Call Name Location
  | If [Resolved] [Resolved] [Resolved] Location
  | PairTerm [Resolved] [Resolved] Location
  | Push Value Location
  | Scoped [Resolved] Location
  | VectorTerm [[Resolved]] Location
  deriving (Eq, Show)

data Value
  = Activation [Value] [Resolved]
  | Bool Bool
  | Char Char
  | Closed Name
  | Closure [ClosedName] [Resolved]
  | Escape Name
  | Float Double
  | Function [Resolved]
  | Handle Handle
  | Int Int
  | Local Name
  | Pair Value Value
  | Unit
  | Vector [Value]
  deriving (Eq, Show)
