module Kitten.Resolved
  ( Resolved(..)
  , Value(..)
  ) where

import System.IO

import Kitten.Anno (Anno)
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Util.Show

data Resolved
  = Block [Resolved]
  | Builtin Builtin Location
  | Call Name Location
  | If [Resolved] [Resolved] [Resolved] Location
  | Push Value Location
  | Scoped [Resolved] Location
  deriving (Eq)

instance Show Resolved where
  show resolved = case resolved of
    Block terms -> showWords terms
    Builtin builtin _ -> show builtin
    Call (Name name) _ -> '@' : show name
    If condition true false _ -> unwords
      [ "if"
      , showWords condition
      , "{"
      , showWords true
      , "} else {"
      , showWords false
      , "}"
      ]
    Push value _ -> show value
    Scoped terms _ -> unwords
      $ "enter" : map show terms ++ ["leave"]

data Value
  = Activation [Value] [Resolved]
  | Bool Bool
  | Char Char
  | Closed Name
  | Closure (Maybe Anno) [ClosedName] [Resolved]
  | Escape Name
  | Float Double
  | Function (Maybe Anno) [Resolved]
  | Handle Handle
  | Int Int
  | Local Name
  | Pair Value Value
  | Unit
  | Vector (Maybe Anno) [Value]
  deriving (Eq)

instance Show Value where
  show v = case v of

    Activation values terms -> concat
      [ "$("
      , showWords values
      , "){"
      , showWords terms
      , "}"
      ]

    Bool value -> if value then "true" else "false"

    Char value -> show value

    Closed (Name index) -> "closed" ++ show index

    Closure _ names terms -> concat
      [ "$("
      , showWords names
      , "){"
      , showWords terms
      , "}"
      ]

    Escape name -> '`' : show name

    Float value -> show value

    Function _ terms -> concat
      [ "(){"
      , showWords terms
      , "}"
      ]

    Handle{} -> "<handle>"

    Int value -> show value

    Local (Name index) -> "local" ++ show index

    Pair a b -> concat ["(", show a, ", ", show b, ")"]

    Unit -> "()"

    Vector _ values -> concat
      [ "["
      , showVector values
      , "]"
      ]

    where
    showVector :: (Show a) => [a] -> String
    showVector = showWords . reverse
