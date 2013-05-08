{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( Located(..)
  , Token(..)
  ) where

import Kitten.Location
import Kitten.Builtin (Builtin)

data Token
  = Arrow
  | BlockBegin
  | BlockEnd
  | Bool Bool
  | BoolType
  | Builtin Builtin
  | Def
  | Else
  | Escape
  | GroupBegin
  | GroupEnd
  | Float Double
  | FloatType
  | If
  | Int Int
  | IntType
  | Lambda
  | Layout
  | Text String
  | TextType
  | VectorBegin
  | VectorEnd
  | Word String
  deriving (Eq)

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BlockBegin -> "{"
    BlockEnd -> "}"
    Bool value -> if value then "true" else "false"
    BoolType -> "bool"
    Builtin name -> show name
    Def -> "def"
    Else -> "else"
    Escape -> "`"
    GroupBegin -> "("
    GroupEnd -> ")"
    Float value -> show value
    FloatType -> "float"
    If -> "if"
    Int value -> show value
    IntType -> "int"
    Lambda -> "\\"
    Layout -> ":"
    Text value -> show value
    TextType -> "text"
    VectorBegin -> "["
    VectorEnd -> "]"
    Word word -> word

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show (Located locatedToken _) = show locatedToken
