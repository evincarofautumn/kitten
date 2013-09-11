{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( BlockTypeHint(..)
  , BaseHint(..)
  , Located(..)
  , Token(..)
  ) where

import Data.Text (Text)
import Numeric

import qualified Data.Text as T

import Kitten.Location
import Kitten.Builtin (Builtin)

data BlockTypeHint
  = NormalBlockHint
  | LayoutBlockHint

data BaseHint
  = BinaryHint
  | OctalHint
  | DecimalHint
  | HexadecimalHint

data Token
  = Arrow
  | BigWord Text
  | BlockBegin BlockTypeHint
  | BlockEnd
  | Bool Bool
  | Builtin Builtin
  | Char Char
  | Choice
  | Comma
  | Def
  | Else
  | From
  | GroupBegin
  | GroupEnd
  | Float Double
  | If
  | Import
  | Int Int BaseHint
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
  Builtin a    == Builtin b    = a == b
  Char a       == Char b       = a == b
  Choice       == Choice       = True
  Comma        == Comma        = True
  Def          == Def          = True
  Else         == Else         = True
  From         == From         = True
  GroupBegin   == GroupBegin   = True
  GroupEnd     == GroupEnd     = True
  Float a      == Float b      = a == b
  If           == If           = True
  Import       == Import       = True
  -- Ints are equal regardless of BaseHint.
  Int a _      == Int b _      = a == b
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
    Builtin name -> show name
    Char char -> show char
    Choice -> "choice"
    Comma -> ","
    Def -> "def"
    Else -> "else"
    From -> "from"
    GroupBegin -> "("
    GroupEnd -> ")"
    Float value -> show value
    If -> "if"
    Import -> "import"
    Int value hint -> if value < 0 then '-' : shown else shown
      where
      shown = prefix ++ showIntAtBase base (digits !!) (abs value) ""
      (base, prefix, digits) = case hint of
        BinaryHint -> (2, "0b", "01")
        OctalHint -> (8, "0o", ['0'..'7'])
        DecimalHint -> (10, "", ['0'..'9'])
        HexadecimalHint -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
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
