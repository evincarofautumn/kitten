{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( BlockTypeHint(..)
  , Located(..)
  , Token(..)
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Location
import Kitten.Builtin (Builtin)

data BlockTypeHint
  = NormalBlockHint
  | LayoutBlockHint

data Token
  = Arrow
  | BigWord Text
  | BlockBegin BlockTypeHint
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

instance Eq Token where
  Arrow        == Arrow        = True
  BigWord a    == BigWord b    = a == b
  -- BlockBegins are equal regardless of BlockTypeHint.
  BlockBegin{} == BlockBegin{} = True
  BlockEnd     == BlockEnd     = True
  Bool a       == Bool b       = a == b
  BoolType     == BoolType     = True
  Builtin a    == Builtin b    = a == b
  Char a       == Char b       = a == b
  CharType     == CharType     = True
  Choice       == Choice       = True
  Comma        == Comma        = True
  Def          == Def          = True
  Else         == Else         = True
  From         == From         = True
  GroupBegin   == GroupBegin   = True
  GroupEnd     == GroupEnd     = True
  Float a      == Float b      = a == b
  FloatType    == FloatType    = True
  HandleType   == HandleType   = True
  IOType       == IOType       = True
  If           == If           = True
  Import       == Import       = True
  Int a        == Int b        = a == b
  IntType      == IntType      = True
  Layout       == Layout       = True
  LittleWord a == LittleWord b = a == b
  Operator a   == Operator b   = a == b
  Option       == Option       = True
  Text a       == Text b       = a == b
  To           == To           = True
  Type         == Type         = True
  VectorBegin  == VectorBegin  = True
  VectorEnd    == VectorEnd    = True
  _            == _            = False

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BigWord word -> T.unpack word
    BlockBegin NormalBlockHint -> "{"
    BlockBegin LayoutBlockHint -> ":"
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
