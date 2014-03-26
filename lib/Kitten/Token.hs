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

import Kitten.Builtin (Builtin)
import Kitten.Location
import Kitten.Util.Text (toText)

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
  | BlockBegin BlockTypeHint
  | BlockEnd
  | Bool Bool
  | Builtin Builtin
  | Char Char
  | Comma
  | Def
  | Do
  | Else
  | GroupBegin
  | GroupEnd
  | Float Double
  | Ignore
  | Import
  | Infix
  | InfixLeft
  | InfixRight
  | Int Int BaseHint
  | Layout
  | Operator Text
  | Postfix
  | Prefix
  | Semicolon
  | Text Text
  | Type
  | VectorBegin
  | VectorEnd
  | Word Text

instance Eq Token where
  Arrow        == Arrow        = True
  -- BlockBegins are equal regardless of BlockTypeHint.
  BlockBegin{} == BlockBegin{} = True
  BlockEnd     == BlockEnd     = True
  Bool a       == Bool b       = a == b
  Builtin a    == Builtin b    = a == b
  Char a       == Char b       = a == b
  Comma        == Comma        = True
  Def          == Def          = True
  Do           == Do           = True
  Else         == Else         = True
  GroupBegin   == GroupBegin   = True
  GroupEnd     == GroupEnd     = True
  Float a      == Float b      = a == b
  Ignore       == Ignore       = True
  Infix        == Infix        = True
  InfixLeft    == InfixLeft    = True
  InfixRight   == InfixRight   = True
  Import       == Import       = True
  -- Ints are equal regardless of BaseHint.
  Int a _      == Int b _      = a == b
  Layout       == Layout       = True
  Operator a   == Operator b   = a == b
  Postfix      == Postfix      = True
  Prefix       == Prefix       = True
  Semicolon    == Semicolon    = True
  Text a       == Text b       = a == b
  Type         == Type         = True
  VectorBegin  == VectorBegin  = True
  VectorEnd    == VectorEnd    = True
  Word a       == Word b       = a == b
  _            == _            = False

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BlockBegin NormalBlockHint -> "{"
    BlockBegin LayoutBlockHint -> ":"
    BlockEnd -> "}"
    Bool value -> if value then "true" else "false"
    Builtin name -> T.unpack (toText name)
    Char char -> show char
    Comma -> ","
    Def -> "def"
    Do -> "\\"
    Else -> "else"
    Ignore -> "_"
    Infix -> "infix"
    InfixLeft -> "infix_left"
    InfixRight -> "infix_right"
    GroupBegin -> "("
    GroupEnd -> ")"
    Float value -> show value
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
    Operator word -> T.unpack word
    Postfix -> "postfix"
    Prefix -> "prefix"
    Semicolon -> ";"
    Text value -> show value
    Type -> "type"
    VectorBegin -> "["
    VectorEnd -> "]"
    Word word -> T.unpack word

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show Located{..} = show locatedToken
