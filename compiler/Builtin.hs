module Builtin
  ( Builtin(..)
  , fromString
  , toString
  ) where

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

import Util

data Builtin
  = Add
  | AndBool
  | AndInt
  | Apply
  | Compose
  | Div
  | Drop
  | Dup
  | Eq
  | Fun
  | Ge
  | Gt
  | If
  | Le
  | Lt
  | Mod
  | Mul
  | Ne
  | Neg
  | NotBool
  | NotInt
  | OrBool
  | OrInt
  | Sub
  | Swap
  | XorBool
  | XorInt
  deriving (Eq, Ord)

instance Show Builtin where show = fromMaybe "<unknown builtin>" . toString

toString :: Builtin -> Maybe String
toString = (`Map.lookup` toStringMap)

fromString :: String -> Maybe Builtin
fromString = (`Map.lookup` fromStringMap)

toStringMap :: Map Builtin String
toStringMap = Map.fromList toStringTable

fromStringMap :: Map String Builtin
fromStringMap = Map.fromList fromStringTable

fromStringTable :: [(String, Builtin)]
fromStringTable =
  [ (,) "+"       Add
  , (,) "&&"      AndBool
  , (,) "&"       AndInt
  , (,) "apply"   Apply
  , (,) "compose" Compose
  , (,) "/"       Div
  , (,) "drop"    Drop
  , (,) "dup"     Dup
  , (,) "="       Eq
  , (,) "fun"     Fun
  , (,) ">="      Ge
  , (,) ">"       Gt
  , (,) "if"      If
  , (,) "<="      Le
  , (,) "<"       Lt
  , (,) "%"       Mod
  , (,) "*"       Mul
  , (,) "!="      Ne
  , (,) "neg"     Neg
  , (,) "!"       NotBool
  , (,) "~"       NotInt
  , (,) "||"      OrBool
  , (,) "|"       OrInt
  , (,) "-"       Sub
  , (,) "swap"    Swap
  , (,) "^^"      XorBool
  , (,) "^"       XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
