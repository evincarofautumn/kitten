{-# LANGUAGE OverloadedStrings #-}

module Kitten.Builtin
  ( Builtin(..)
  , fromText
  , names
  , toText
  ) where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as Text

import Kitten.Util

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
  | Fun
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
  | Vec
  | XorBool
  | XorInt
  deriving (Eq, Ord)

instance Show Builtin where
  show = maybe "<unknown builtin>" Text.unpack . toText

toText :: Builtin -> Maybe Text
toText = (`Map.lookup` toTextMap)

fromText :: Text -> Maybe Builtin
fromText = (`Map.lookup` fromTextMap)

toTextMap :: Map Builtin Text
toTextMap = Map.fromList toTextTable

fromTextMap :: Map Text Builtin
fromTextMap = Map.fromList fromTextTable

names :: [Text]
names = map fst fromTextTable

fromTextTable :: [(Text, Builtin)]
fromTextTable =
  [ (,) "+"       Add
  , (,) "&&"      AndBool
  , (,) "&"       AndInt
  , (,) "apply"   Apply
  , (,) "@"       At
  , (,) "bottom"  Bottom
  , (,) "cat"     Cat
  , (,) "compose" Compose
  , (,) "/"       Div
  , (,) "down"    Down
  , (,) "drop"    Drop
  , (,) "dup"     Dup
  , (,) "="       Eq
  , (,) "empty"   Empty
  , (,) "fun"     Fun
  , (,) ">="      Ge
  , (,) ">"       Gt
  , (,) "if"      If
  , (,) "<="      Le
  , (,) "length"  Length
  , (,) "<"       Lt
  , (,) "%"       Mod
  , (,) "*"       Mul
  , (,) "!="      Ne
  , (,) "neg"     Neg
  , (,) "!"       NotBool
  , (,) "~"       NotInt
  , (,) "||"      OrBool
  , (,) "|"       OrInt
  , (,) "print"   Print
  , (,) "-"       Sub
  , (,) "swap"    Swap
  , (,) "top"     Top
  , (,) "up"      Up
  , (,) "vec"     Vec
  , (,) "^^"      XorBool
  , (,) "^"       XorInt
  ]

toTextTable :: [(Builtin, Text)]
toTextTable = map swap fromTextTable
