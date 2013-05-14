{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( Located(..)
  , Token(..)
  ) where

import Kitten.Location
import Kitten.Builtin (Builtin)

data Token
  = Arrow
  | BigWord String
  | BlockBegin
  | BlockEnd
  | Bool Bool
  | BoolType
  | Builtin Builtin
  | Char Char
  | CharType
  | Comma
  | Def
  | Else
  | Escape
  | GroupBegin
  | GroupEnd
  | Float Double
  | FloatType
  | HandleType
  | If
  | Int Int
  | IntType
  | Layout
  | LittleWord String
  | Text String
  | VectorBegin
  | VectorEnd
  deriving (Eq)

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BigWord word -> word
    BlockBegin -> "{"
    BlockEnd -> "}"
    Bool value -> if value then "true" else "false"
    BoolType -> "Bool"
    Builtin name -> show name
    Char char -> show char
    CharType -> "Char"
    Comma -> ","
    Def -> "def"
    Else -> "else"
    Escape -> "`"
    GroupBegin -> "("
    GroupEnd -> ")"
    Float value -> show value
    FloatType -> "Float"
    HandleType -> "Handle"
    If -> "if"
    Int value -> show value
    IntType -> "Int"
    Layout -> ":"
    LittleWord word -> word
    Text value -> show value
    VectorBegin -> "["
    VectorEnd -> "]"

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show (Located locatedToken _) = show locatedToken
