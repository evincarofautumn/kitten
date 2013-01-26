{-# LANGUAGE OverloadedStrings #-}

module Kitten.Builtin
  ( Builtin(..)
  , fromText
  , toText
  ) where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map

import Kitten.Util

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
  | Print
  | Sub
  | Swap
  | XorBool
  | XorInt
  deriving (Eq, Ord)

instance Show Builtin where show = maybe "<unknown builtin>" show . toText

toText :: Builtin -> Maybe Text
toText = (`Map.lookup` toTextMap)

fromText :: Text -> Maybe Builtin
fromText = (`Map.lookup` fromTextMap)

toTextMap :: Map Builtin Text
toTextMap = Map.fromList toTextTable

fromTextMap :: Map Text Builtin
fromTextMap = Map.fromList fromTextTable

fromTextTable :: [(Text, Builtin)]
fromTextTable =
  [ (,) "+"       Add
  , (,) "&&"      AndBool
  , (,) "&"       AndInt
  , (,) "apply"   Apply
  , (,) "compose" Compose
  , (,) "/"       Div
  , (,) "drop"    Drop
  , (,) "dup"     Dup
  , (,) "="       Eq
  , (,) "fun"     Fun
  , (,) ">="      Ge
  , (,) ">"       Gt
  , (,) "if"      If
  , (,) "<="      Le
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
  , (,) "^^"      XorBool
  , (,) "^"       XorInt
  ]

toTextTable :: [(Builtin, Text)]
toTextTable = map swap fromTextTable
