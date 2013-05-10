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
  = AddFloat
  | AddInt
  | AndBool
  | AndInt
  | Apply
  | At
  | Bottom
  | Cat
  | Compose
  | DecFloat
  | DecInt
  | DivFloat
  | DivInt
  | Down
  | EqFloat
  | EqInt
  | Empty
  | Function
  | GeFloat
  | GeInt
  | GtFloat
  | GtInt
  | IncFloat
  | IncInt
  | LeFloat
  | LeInt
  | Length
  | LtFloat
  | LtInt
  | ModFloat
  | ModInt
  | MulFloat
  | MulInt
  | NeFloat
  | NeInt
  | NegFloat
  | NegInt
  | NotBool
  | NotInt
  | OrBool
  | OrInt
  | Print
  | ShowFloat
  | ShowInt
  | SubFloat
  | SubInt
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
  [ (,) "__add_float"  AddFloat
  , (,) "__add_int"    AddInt
  , (,) "__and_bool"   AndBool
  , (,) "__and_int"    AndInt
  , (,) "__apply"      Apply
  , (,) "__at"         At
  , (,) "__bottom"     Bottom
  , (,) "__cat"        Cat
  , (,) "__compose"    Compose
  , (,) "__dec_float"  DecFloat
  , (,) "__dec_int"    DecInt
  , (,) "__div_float"  DivFloat
  , (,) "__div_int"    DivInt
  , (,) "__down"       Down
  , (,) "__eq_float"   EqFloat
  , (,) "__eq_int"     EqInt
  , (,) "__empty"      Empty
  , (,) "__function"   Function
  , (,) "__ge_float"   GeFloat
  , (,) "__ge_int"     GeInt
  , (,) "__gt_float"   GtFloat
  , (,) "__gt_int"     GtInt
  , (,) "__inc_float"  IncFloat
  , (,) "__inc_int"    IncInt
  , (,) "__le_float"   LeFloat
  , (,) "__le_int"     LeInt
  , (,) "__length"     Length
  , (,) "__lt_float"   LtFloat
  , (,) "__lt_int"     LtInt
  , (,) "__mod_float"  ModFloat
  , (,) "__mod_int"    ModInt
  , (,) "__mul_float"  MulFloat
  , (,) "__mul_int"    MulInt
  , (,) "__ne_float"   NeFloat
  , (,) "__ne_int"     NeInt
  , (,) "__neg_float"  NegFloat
  , (,) "__neg_int"    NegInt
  , (,) "__not_bool"   NotBool
  , (,) "__not_int"    NotInt
  , (,) "__or_bool"    OrBool
  , (,) "__or_int"     OrInt
  , (,) "__print"      Print
  , (,) "__show_float" ShowFloat
  , (,) "__show_int"   ShowInt
  , (,) "__sub_float"  SubFloat
  , (,) "__sub_int"    SubInt
  , (,) "__top"        Top
  , (,) "__up"         Up
  , (,) "__vector"     Vector
  , (,) "__xor_bool"   XorBool
  , (,) "__xor_int"    XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
