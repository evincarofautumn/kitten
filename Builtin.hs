module Builtin
  ( Builtin(..)
  , fromString
  , toString
  ) where

import Prelude hiding (Bool(..))

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

import Util

data Builtin
  = Add
  | And
  | Apply
  | Compose
  | Div
  | Drop
  | Dup
  | Eq
  | False
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
  | Not
  | Or
  | Sub
  | Swap
  | True
  | Xor
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
  , (,) "and"     And
  , (,) "apply"   Apply
  , (,) "compose" Compose
  , (,) "div"     Div
  , (,) "drop"    Drop
  , (,) "dup"     Dup
  , (,) "eq"      Eq
  , (,) "false"   False
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
  , (,) "not"     Not
  , (,) "or"      Or
  , (,) "sub"     Sub
  , (,) "swap"    Swap
  , (,) "true"    True
  , (,) "xor"     Xor
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
