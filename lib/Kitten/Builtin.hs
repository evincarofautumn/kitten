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
  | ShowInt
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
  [ (,) "__add_int"   Add
  , (,) "__and_bool"  AndBool
  , (,) "__and_int"   AndInt
  , (,) "__apply"     Apply
  , (,) "__at"        At
  , (,) "__bottom"    Bottom
  , (,) "__cat"       Cat
  , (,) "__compose"   Compose
  , (,) "__div_int"   Div
  , (,) "__down"      Down
  , (,) "__drop"      Drop
  , (,) "__dup"       Dup
  , (,) "__eq_int"    Eq
  , (,) "__empty"     Empty
  , (,) "__function"  Function
  , (,) "__ge_int"    Ge
  , (,) "__gt_int"    Gt
  , (,) "__le_int"    Le
  , (,) "__length"    Length
  , (,) "__lt_int"    Lt
  , (,) "__mod_int"   Mod
  , (,) "__mul_int"   Mul
  , (,) "__ne_int"    Ne
  , (,) "__neg_int"   Neg
  , (,) "__not_bool"  NotBool
  , (,) "__not_int"   NotInt
  , (,) "__or_bool"   OrBool
  , (,) "__or_int"    OrInt
  , (,) "__print"     Print
  , (,) "__show_int"  ShowInt
  , (,) "__sub_int"   Sub
  , (,) "__swap"      Swap
  , (,) "__top"       Top
  , (,) "__up"        Up
  , (,) "__vector"    Vector
  , (,) "__xor_bool"  XorBool
  , (,) "__xor_int"   XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
