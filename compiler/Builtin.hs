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
  [ (,) "add"     Add
  , (,) "and"     AndBool
  , (,) "bitand"  AndInt
  , (,) "apply"   Apply
  , (,) "compose" Compose
  , (,) "div"     Div
  , (,) "drop"    Drop
  , (,) "dup"     Dup
  , (,) "eq"      Eq
  , (,) "fun"     Fun
  , (,) "ge"      Ge
  , (,) "gt"      Gt
  , (,) "if"      If
  , (,) "le"      Le
  , (,) "lt"      Lt
  , (,) "mod"     Mod
  , (,) "mul"     Mul
  , (,) "ne"      Ne
  , (,) "neg"     Neg
  , (,) "not"     NotBool
  , (,) "bitnot"  NotInt
  , (,) "or"      OrBool
  , (,) "bitor"   OrInt
  , (,) "sub"     Sub
  , (,) "swap"    Swap
  , (,) "xor"     XorBool
  , (,) "bitxor"  XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
