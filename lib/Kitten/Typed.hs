module Kitten.Typed
  ( Typed(..)
  , Value(..)
  ) where

import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Util.Show

data Typed
  = Call Name Location
  | Compose [Typed]
  | Builtin Builtin Location
  | If Typed Typed Location
  | Push Value Location
  | Scoped Typed Location
  deriving (Eq)

instance Show Typed where
  show typed = case typed of
    Call name _ -> show name
    Compose terms -> showWords terms
    Builtin builtin _ -> show builtin
    If true false _ -> "if { " ++ show true ++ " } else { " ++ show false ++ " }"
    Push value _ -> "(" ++ show value ++ ")"
    Scoped term _ -> "enter " ++ show term ++ " leave"

data Value
  = Activation [Value] Typed
  | Bool Bool
  | Char Char
  | Closed Name
  | Closure [ClosedName] Typed
  | Escape Name
  | Float Double
  | Function Typed
  | Handle Handle
  | Int Int
  | Local Name
  | Pair Value Value
  | Unit
  | Vector [Value]
  deriving (Eq, Show)
