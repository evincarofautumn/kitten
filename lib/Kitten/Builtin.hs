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
  | Bottom
  | CatVector
  | Compose
  | DecFloat
  | DecInt
  | DivFloat
  | DivInt
  | Down
  | Drop
  | Dup
  | Empty
  | EqFloat
  | EqInt
  | EqVector
  | Function
  | GeFloat
  | GeInt
  | GeVector
  | Get
  | GetLine
  | GtFloat
  | GtInt
  | GtVector
  | IncFloat
  | IncInt
  | LeFloat
  | LeInt
  | LeVector
  | Length
  | LtFloat
  | LtInt
  | LtVector
  | ModFloat
  | ModInt
  | MulFloat
  | MulInt
  | NeFloat
  | NeInt
  | NeVector
  | NegFloat
  | NegInt
  | NotBool
  | NotInt
  | OrBool
  | OrInt
  | Print
  | Set
  | ShowFloat
  | ShowInt
  | SubFloat
  | SubInt
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
  [ (,) "__add_float"  AddFloat
  , (,) "__add_int"    AddInt
  , (,) "__and_bool"   AndBool
  , (,) "__and_int"    AndInt
  , (,) "__apply"      Apply
  , (,) "__bottom"     Bottom
  , (,) "__cat_vector" CatVector
  , (,) "__compose"    Compose
  , (,) "__dec_float"  DecFloat
  , (,) "__dec_int"    DecInt
  , (,) "__div_float"  DivFloat
  , (,) "__div_int"    DivInt
  , (,) "__down"       Down
  , (,) "__drop"       Drop
  , (,) "__dup"        Dup
  , (,) "__eq_float"   EqFloat
  , (,) "__eq_int"     EqInt
  , (,) "__eq_vector"  EqVector
  , (,) "__empty"      Empty
  , (,) "__function"   Function
  , (,) "__ge_float"   GeFloat
  , (,) "__ge_int"     GeInt
  , (,) "__ge_vector"  GeVector
  , (,) "__get"        Get
  , (,) "__get_line"   GetLine
  , (,) "__gt_float"   GtFloat
  , (,) "__gt_int"     GtInt
  , (,) "__gt_vector"  GtVector
  , (,) "__inc_float"  IncFloat
  , (,) "__inc_int"    IncInt
  , (,) "__le_float"   LeFloat
  , (,) "__le_int"     LeInt
  , (,) "__le_vector"  LeVector
  , (,) "__length"     Length
  , (,) "__lt_float"   LtFloat
  , (,) "__lt_int"     LtInt
  , (,) "__lt_vector"  LtVector
  , (,) "__mod_float"  ModFloat
  , (,) "__mod_int"    ModInt
  , (,) "__mul_float"  MulFloat
  , (,) "__mul_int"    MulInt
  , (,) "__ne_float"   NeFloat
  , (,) "__ne_int"     NeInt
  , (,) "__ne_vector"  NeVector
  , (,) "__neg_float"  NegFloat
  , (,) "__neg_int"    NegInt
  , (,) "__not_bool"   NotBool
  , (,) "__not_int"    NotInt
  , (,) "__or_bool"    OrBool
  , (,) "__or_int"     OrInt
  , (,) "__print"      Print
  , (,) "__set"        Set
  , (,) "__show_float" ShowFloat
  , (,) "__show_int"   ShowInt
  , (,) "__sub_float"  SubFloat
  , (,) "__sub_int"    SubInt
  , (,) "__swap"       Swap
  , (,) "__top"        Top
  , (,) "__up"         Up
  , (,) "__vector"     Vector
  , (,) "__xor_bool"   XorBool
  , (,) "__xor_int"    XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
