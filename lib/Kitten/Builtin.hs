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
  | AddVector
  | AndBool
  | AndInt
  | Apply01
  | Apply10
  | Apply11
  | Apply21
  | Bottom
  | Close
  | DecFloat
  | DecInt
  | DivFloat
  | DivInt
  | Down
  | Drop
  | Dup
  | Empty
  | EqChar
  | EqFloat
  | EqInt
  | EqVector
  | Exit
  | First
  | Function
  | GeChar
  | GeFloat
  | GeInt
  | GeVector
  | Get
  | GetLine
  | GtChar
  | GtFloat
  | GtInt
  | GtVector
  | IncFloat
  | IncInt
  | LeChar
  | LeFloat
  | LeInt
  | LeVector
  | Length
  | LtChar
  | LtFloat
  | LtInt
  | LtVector
  | ModFloat
  | ModInt
  | MulFloat
  | MulInt
  | NeChar
  | NeFloat
  | NeInt
  | NeVector
  | NegFloat
  | NegInt
  | NotBool
  | NotInt
  | OpenIn
  | OpenOut
  | OrBool
  | OrInt
  | Print
  | Rest
  | Set
  | ShowFloat
  | ShowInt
  | SubFloat
  | SubInt
  | Stderr
  | Stdin
  | Stdout
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
  , (,) "__add_vector" AddVector
  , (,) "__and_bool"   AndBool
  , (,) "__and_int"    AndInt
  , (,) "__apply01"    Apply01
  , (,) "__apply10"    Apply10
  , (,) "__apply11"    Apply11
  , (,) "__apply21"    Apply21
  , (,) "__bottom"     Bottom
  , (,) "__close"      Close
  , (,) "__dec_float"  DecFloat
  , (,) "__dec_int"    DecInt
  , (,) "__div_float"  DivFloat
  , (,) "__div_int"    DivInt
  , (,) "__down"       Down
  , (,) "__drop"       Drop
  , (,) "__dup"        Dup
  , (,) "__eq_char"    EqChar
  , (,) "__eq_float"   EqFloat
  , (,) "__eq_int"     EqInt
  , (,) "__eq_vector"  EqVector
  , (,) "__exit"       Exit
  , (,) "__empty"      Empty
  , (,) "__first"      First
  , (,) "__function"   Function
  , (,) "__ge_char"    GeChar
  , (,) "__ge_float"   GeFloat
  , (,) "__ge_int"     GeInt
  , (,) "__ge_vector"  GeVector
  , (,) "__get"        Get
  , (,) "__get_line"   GetLine
  , (,) "__gt_char"    GtChar
  , (,) "__gt_float"   GtFloat
  , (,) "__gt_int"     GtInt
  , (,) "__gt_vector"  GtVector
  , (,) "__inc_float"  IncFloat
  , (,) "__inc_int"    IncInt
  , (,) "__le_char"    LeChar
  , (,) "__le_float"   LeFloat
  , (,) "__le_int"     LeInt
  , (,) "__le_vector"  LeVector
  , (,) "__length"     Length
  , (,) "__lt_char"    LtChar
  , (,) "__lt_float"   LtFloat
  , (,) "__lt_int"     LtInt
  , (,) "__lt_vector"  LtVector
  , (,) "__mod_float"  ModFloat
  , (,) "__mod_int"    ModInt
  , (,) "__mul_float"  MulFloat
  , (,) "__mul_int"    MulInt
  , (,) "__ne_char"    NeChar
  , (,) "__ne_float"   NeFloat
  , (,) "__ne_int"     NeInt
  , (,) "__ne_vector"  NeVector
  , (,) "__neg_float"  NegFloat
  , (,) "__neg_int"    NegInt
  , (,) "__not_bool"   NotBool
  , (,) "__not_int"    NotInt
  , (,) "__open_in"    OpenIn
  , (,) "__open_out"   OpenOut
  , (,) "__or_bool"    OrBool
  , (,) "__or_int"     OrInt
  , (,) "__print"      Print
  , (,) "__rest"       Rest
  , (,) "__set"        Set
  , (,) "__show_float" ShowFloat
  , (,) "__show_int"   ShowInt
  , (,) "__sub_float"  SubFloat
  , (,) "__sub_int"    SubInt
  , (,) "__stderr"     Stderr
  , (,) "__stdin"      Stdin
  , (,) "__stdout"     Stdout
  , (,) "__swap"       Swap
  , (,) "__top"        Top
  , (,) "__up"         Up
  , (,) "__vector"     Vector
  , (,) "__xor_bool"   XorBool
  , (,) "__xor_int"    XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
