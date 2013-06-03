module Kitten.Resolved
  ( ClosedName(..)
  , Resolved(..)
  , Value(..)
  ) where

import Data.Set (Set)
import System.IO

import qualified Data.Set as Set

import Kitten.Anno (Anno)
import Kitten.Builtin (Builtin)
import Kitten.Location
import Kitten.Name
import Kitten.Util.Show

data Resolved
  = Block [Resolved]
  | Builtin Builtin Location
  | Closed Name Location
  | If [Resolved] [Resolved] [Resolved] Location
  | Local Name Location
  | Push Value Location
  | Scoped [Resolved] Location
  deriving (Eq)

instance Show Resolved where
  show resolved = case resolved of
    Block terms -> showWords terms
    Builtin builtin _ -> show builtin
    Closed (Name index) _ -> "closed" ++ show index
    If condition true false _ -> unwords
      [ "if"
      , showWords condition
      , "then"
      , showWords true
      , "else"
      , showWords false
      ]
    Local (Name index) _ -> "local" ++ show index
    Push value _ -> show value
    Scoped terms _ -> unwords $ "\\" : map show terms

data Value
  = Activation [Value] [Resolved]
  | Bool Bool
  | Char Char
  | Closure Anno [ClosedName] [Resolved]
  | Escape (Set Name)
  | Float Double
  | Function Anno [Resolved]
  | Handle Handle
  | Int Int
  | Pair Value Value
  | Unit
  | Vector (Maybe Anno) [Value]
  | Word (Set Name)
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

    Closure _ names terms -> concat
      [ "$("
      , showWords names
      , "){"
      , showWords terms
      , "}"
      ]

    Escape possible -> '`' : showPossible possible

    Float value -> show value

    Function anno terms -> concat
      [ "("
      , show anno
      , "){"
      , showWords terms
      , "}"
      ]

    Handle{} -> "<handle>"

    Int value -> show value

    Pair a b -> concat ["(", show a, ", ", show b, ")"]

    Unit -> "()"

    Vector anno values -> concat
      [ maybe "" (("(" ++) . (++ ")") . show) anno
      , "["
      , showVector values
      , "]"
      ]

    Word possible -> '@' : showPossible possible

    where
    showVector :: (Show a) => [a] -> String
    showVector = showWords . reverse

    showPossible :: (Ord a, Show a) => Set a -> String
    showPossible possible = case Set.toList possible of
      [name] -> '@' : show name
      names -> "@{" ++ showWords names ++ "}"

data ClosedName
  = ClosedName Name
  | ReclosedName Name
  deriving (Eq, Show)
