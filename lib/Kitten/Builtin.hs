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
  | Apply
  | CharToInt
  | Close
  | DivFloat
  | DivInt
  | EqFloat
  | EqInt
  | Exit
  | First
  | FromLeft
  | FromRight
  | FromSome
  | GeFloat
  | GeInt
  | Get
  | GetLine
  | GtFloat
  | GtInt
  | Impure
  | Init
  | IsNone
  | IsRight
  | LeFloat
  | LeInt
  | Left
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
  | None
  | NotBool
  | NotInt
  | OpenIn
  | OpenOut
  | OrBool
  | OrInt
  | Pair
  | Print
  | Rest
  | Right
  | Set
  | ShowFloat
  | ShowInt
  | Some
  | Stderr
  | Stdin
  | Stdout
  | SubFloat
  | SubInt
  | Tail
  | UnsafePurify11
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
  [ (,) "@"                 Apply
  , (,) "__add_float"       AddFloat
  , (,) "__add_int"         AddInt
  , (,) "__add_vector"      AddVector
  , (,) "__and_bool"        AndBool
  , (,) "__and_int"         AndInt
  , (,) "__char_to_int"     CharToInt
  , (,) "__close"           Close
  , (,) "__div_float"       DivFloat
  , (,) "__div_int"         DivInt
  , (,) "__eq_float"        EqFloat
  , (,) "__eq_int"          EqInt
  , (,) "__exit"            Exit
  , (,) "__first"           First
  , (,) "__from_left"       FromLeft
  , (,) "__from_right"      FromRight
  , (,) "__from_some"       FromSome
  , (,) "__ge_float"        GeFloat
  , (,) "__ge_int"          GeInt
  , (,) "__get"             Get
  , (,) "__get_line"        GetLine
  , (,) "__gt_float"        GtFloat
  , (,) "__gt_int"          GtInt
  , (,) "__impure"          Impure
  , (,) "__init"            Init
  , (,) "__is_none"         IsNone
  , (,) "__is_right"        IsRight
  , (,) "__le_float"        LeFloat
  , (,) "__le_int"          LeInt
  , (,) "__left"            Kitten.Builtin.Left
  , (,) "__length"          Length
  , (,) "__lt_float"        LtFloat
  , (,) "__lt_int"          LtInt
  , (,) "__mod_float"       ModFloat
  , (,) "__mod_int"         ModInt
  , (,) "__mul_float"       MulFloat
  , (,) "__mul_int"         MulInt
  , (,) "__ne_float"        NeFloat
  , (,) "__ne_int"          NeInt
  , (,) "__neg_float"       NegFloat
  , (,) "__neg_int"         NegInt
  , (,) "__none"            None
  , (,) "__not_bool"        NotBool
  , (,) "__not_int"         NotInt
  , (,) "__open_in"         OpenIn
  , (,) "__open_out"        OpenOut
  , (,) "__or_bool"         OrBool
  , (,) "__or_int"          OrInt
  , (,) "__pair"            Pair
  , (,) "__print"           Print
  , (,) "__rest"            Rest
  , (,) "__right"           Kitten.Builtin.Right
  , (,) "__set"             Set
  , (,) "__show_float"      ShowFloat
  , (,) "__show_int"        ShowInt
  , (,) "__some"            Some
  , (,) "__stderr"          Stderr
  , (,) "__stdin"           Stdin
  , (,) "__stdout"          Stdout
  , (,) "__sub_float"       SubFloat
  , (,) "__sub_int"         SubInt
  , (,) "__tail"            Tail
  , (,) "__unsafe_purify11" UnsafePurify11
  , (,) "__vector"          Vector
  , (,) "__xor_bool"        XorBool
  , (,) "__xor_int"         XorInt
  ]

toStringTable :: [(Builtin, String)]
toStringTable = map swap fromStringTable
