{-# LANGUAGE OverloadedStrings #-}

module Kitten.Builtin
  ( Builtin(..)
  , fromString
  , names
  , toString
  ) where

import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as Map

import Kitten.Util.Tuple

data Builtin
  = Add
  | AndBool
  | AndInt
  | Apply
  | At
  | Bottom
  | Cat
  | Compose
  | Div
  | Down
  | Drop
  | Dup
  | Eq
  | Empty
  | Function
  | Ge
  | Gt
  | If
  | Le
  | Length
  | Lt
  | Mod
  | Mul
  | Ne
  | Neg
  | NotBool
  | NotInt
  | OrBool
  | OrInt
  | Print
  | Sub
  | Swap
  | Top
  | Up
  | Vector
  | XorBool
  | XorInt
  deriving (Eq, Ord)

instance Show Builtin where
  show = fromMaybe "<unknown builtin>" . toString

toString :: Builtin -> Maybe String
toString = (`Map.lookup` toStringMap)

fromString :: String -> Maybe Builtin
fromString = (`Map.lookup` fromStringMap)

toStringMap :: Map Builtin String
toStringMap = Map.fromList toStringTable

fromStringMap :: Map String Builtin
fromStringMap = Map.fromList fromStringTable

names :: [String]
names = map fst fromStringTable

fromStringTable :: [(String, Builtin)]
fromStringTable =
  [ (,) "+"        Add
  , (,) "&&"       AndBool
  , (,) "&"        AndInt
  , (,) "apply"    Apply
  , (,) "@"        At
  , (,) "bottom"   Bottom
  , (,) "cat"      Cat
  , (,) "compose"  Compose
  , (,) "/"        Div
  , (,) "down"     Down
  , (,) "drop"     Drop
  , (,) "dup"      Dup
  , (,) "="        Eq
  , (,) "empty"    Empty
  , (,) "function" Function
  , (,) ">="       Ge
  , (,) ">"        Gt
  , (,) "if"       If
  , (,) "<="       Le
  , (,) "length"   Length
  , (,) "<"        Lt
  , (,) "%"        Mod
  , (,) "*"        Mul
  , (,) "!="       Ne
  , (,) "neg"      Neg
  , (,) "!"        NotBool
  , (,) "~"        NotInt
  , (,) "||"       OrBool
  , (,) "|"        OrInt
  , (,) "print"    Print
  , (,) "-"        Sub
  , (,) "swap"     Swap
  , (,) "top"      Top
  , (,) "up"       Up
  , (,) "vector"   Vector
  , (,) "^^"       XorBool
  , (,) "^"        XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
