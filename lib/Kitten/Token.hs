{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( Located(..)
  , Token(..)
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Location
import Kitten.Builtin (Builtin)

data Token
  = Arrow
  | BigWord Text
  | BlockBegin
  | BlockEnd
  | Bool Bool
  | BoolType
  | Builtin Builtin
  | Char Char
  | CharType
  | Choice
  | Comma
  | Def
  | Else
  | From
  | GroupBegin
  | GroupEnd
  | Float Double
  | FloatType
  | HandleType
  | IOType
  | If
  | Import
  | Int Int
  | IntType
  | Layout
  | LittleWord Text
  | Operator Text
  | Option
  | Text Text
  | To
  | Type
  | VectorBegin
  | VectorEnd
  deriving (Eq)

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BigWord word -> T.unpack word
    BlockBegin -> "{"
    BlockEnd -> "}"
    Bool value -> if value then "true" else "false"
    BoolType -> "Bool"
    Builtin name -> show name
    Char char -> show char
    CharType -> "Char"
    Choice -> "choice"
    Comma -> ","
    Def -> "def"
    Else -> "else"
    From -> "from"
    GroupBegin -> "("
    GroupEnd -> ")"
    Float value -> show value
    FloatType -> "Float"
    HandleType -> "Handle"
    IOType -> "IO"
    If -> "if"
    Import -> "import"
    Int value -> show value
    IntType -> "Int"
    Layout -> ":"
    LittleWord word -> T.unpack word
    Operator word -> T.unpack word
    Option -> "option"
    Text value -> show value
    To -> "to"
    Type -> "type"
    VectorBegin -> "["
    VectorEnd -> "]"

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show Located{..} = show locatedToken
