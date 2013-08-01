module Kitten.Resolved
  ( Resolved(..)
  , Value(..)
  , charsFromString
  , stringFromChars
  ) where

import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name

data Resolved
  = Builtin Builtin Location
  | Call Name Location
  | ChoiceTerm Resolved Resolved Location
  | Compose [Resolved] Location
  | Group [Resolved] Location
  | If Resolved Resolved Location
  | OptionTerm Resolved Resolved Location
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
  deriving (Eq)

instance Show Value where
  show value = case value of
    Activation{} -> "<function>"
    Bool b -> if b then "true" else "false"
    Char c -> show c
    Choice which v -> unwords
      [show v, if which then "right" else "left"]
    Closed{} -> "<closed>"
    Closure{} -> "<function>"
    Float f -> show f
    Function{} -> "<function>"
    Handle{} -> "<handle>"
    Int i -> show i
    Local{} -> "<local>"
    Option m -> maybe "none" ((++ " some") . show) m
    Pair a b -> unwords [show a, show b, "pair"]
    Unit -> "()"
    Vector v@(Char _ : _) -> show (stringFromChars v)
    Vector v -> show v

stringFromChars :: [Value] -> String
stringFromChars = map fromChar
  where
  fromChar :: Value -> Char
  fromChar (Char c) = c
  fromChar _ = error "stringFromChars: non-character"

charsFromString :: String -> [Value]
charsFromString = map Char
