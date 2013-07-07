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
  | Close
  | DecFloat
  | DecInt
  | DivFloat
  | DivInt
  | Empty
  | EqChar
  | EqFloat
  | EqInt
  | Exit
  | First
  | GeChar
  | GeFloat
  | GeInt
  | Get
  | GetLine
  | GtChar
  | GtFloat
  | GtInt
  | Head
  | IncFloat
  | IncInt
  | Init
  | Last
  | LeChar
  | LeFloat
  | LeInt
  | Length
  | LtChar
  | LtFloat
  | LtInt
  | ModFloat
  | ModInt
  | MulFloat
  | MulInt
  | NeChar
  | NeFloat
  | NeInt
  | NegFloat
  | NegInt
  | NotBool
  | NotInt
  | OpenIn
  | OpenOut
  | OrBool
  | OrInt
  | Pair
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
  | Tail
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
  , (,) "__close"      Close
  , (,) "__dec_float"  DecFloat
  , (,) "__dec_int"    DecInt
  , (,) "__div_float"  DivFloat
  , (,) "__div_int"    DivInt
  , (,) "__eq_char"    EqChar
  , (,) "__eq_float"   EqFloat
  , (,) "__eq_int"     EqInt
  , (,) "__exit"       Exit
  , (,) "__empty"      Empty
  , (,) "__first"      First
  , (,) "__ge_char"    GeChar
  , (,) "__ge_float"   GeFloat
  , (,) "__ge_int"     GeInt
  , (,) "__get"        Get
  , (,) "__get_line"   GetLine
  , (,) "__gt_char"    GtChar
  , (,) "__gt_float"   GtFloat
  , (,) "__gt_int"     GtInt
  , (,) "__head"       Head
  , (,) "__inc_float"  IncFloat
  , (,) "__inc_int"    IncInt
  , (,) "__init"       Init
  , (,) "__last"       Last
  , (,) "__le_char"    LeChar
  , (,) "__le_float"   LeFloat
  , (,) "__le_int"     LeInt
  , (,) "__length"     Length
  , (,) "__lt_char"    LtChar
  , (,) "__lt_float"   LtFloat
  , (,) "__lt_int"     LtInt
  , (,) "__mod_float"  ModFloat
  , (,) "__mod_int"    ModInt
  , (,) "__mul_float"  MulFloat
  , (,) "__mul_int"    MulInt
  , (,) "__ne_char"    NeChar
  , (,) "__ne_float"   NeFloat
  , (,) "__ne_int"     NeInt
  , (,) "__neg_float"  NegFloat
  , (,) "__neg_int"    NegInt
  , (,) "__not_bool"   NotBool
  , (,) "__not_int"    NotInt
  , (,) "__open_in"    OpenIn
  , (,) "__open_out"   OpenOut
  , (,) "__or_bool"    OrBool
  , (,) "__or_int"     OrInt
  , (,) "__pair"       Pair
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
  , (,) "__tail"       Tail
  , (,) "__vector"     Vector
  , (,) "__xor_bool"   XorBool
  , (,) "__xor_int"    XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
