{-# LANGUAGE OverloadedStrings #-}

module Kitten.Token
  ( Token(..)
  , float
  ) where

import Data.Ratio ((%))
import Data.Text (Text)
import Kitten.Base (Base(..))
import Kitten.Layoutness (Layoutness)
import Kitten.Name (Unqualified)
import Numeric
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Text.PrettyPrint as Pretty

data Token
  = About                     -- about
  | AngleBegin                -- < See note [Angle Brackets].
  | AngleEnd                  -- > See note [Angle Brackets].
  | Arrow                     -- ->
  | BlockBegin !Layoutness    -- { :
  | BlockEnd                  -- }
  | Call                      -- call
  | Case                      -- case
  | Character !Char           -- 'x'
  | Comma                     -- ,
  | Define                    -- define
  | Do                        -- do
  | Ellipsis                  -- ...
  | Elif                      -- elif
  | Else                      -- else
  | Float !Integer !Int !Int  -- See note [Float Literals].
  | GroupBegin                -- (
  | GroupEnd                  -- )
  | If                        -- if
  | Ignore                    -- _
  | Infix                     -- infix
  | Instance                  -- instance
  | Integer !Integer !Base    -- 1 0b1 0o1 0x1
  | Jump                      -- jump
  | Layout                    -- :
  | Match                     -- match
  | Operator !Unqualified     -- +
  | Permission                -- permission
  | Reference                 -- \
  | Return                    -- return
  | Synonym                   -- synonym
  | Text !Text                -- "..."
  | Trait                     -- trait
  | Type                      -- type
  | VectorBegin               -- [
  | VectorEnd                 -- ]
  | Vocab                     -- vocab
  | VocabLookup               -- ::
  | Word !Unqualified         -- word

instance Eq Token where
  About                   == About                   = True
  AngleBegin              == AngleBegin              = True
  AngleEnd                == AngleEnd                = True
  Arrow                   == Arrow                   = True
  -- Block begin tokens are equal regardless of layoutness.
  BlockBegin _layoutnessA == BlockBegin _layoutnessB = True
  BlockEnd                == BlockEnd                = True
  Call                    == Call                    = True
  Case                    == Case                    = True
  Character a             == Character b             = a == b
  Comma                   == Comma                   = True
  Define                  == Define                  = True
  Do                      == Do                      = True
  Ellipsis                == Ellipsis                = True
  Elif                    == Elif                    = True
  Else                    == Else                    = True
  -- See note [Float Literals].
  Float a b c             == Float d e f             = (a, c - b) == (d, f - e)
  GroupBegin              == GroupBegin              = True
  GroupEnd                == GroupEnd                = True
  If                      == If                      = True
  Ignore                  == Ignore                  = True
  Infix                   == Infix                   = True
  Instance                == Instance                = True
  -- Integer tokens are equal regardless of base.
  Integer a _baseA        == Integer b _baseB        = a == b
  Jump                    == Jump                    = True
  Layout                  == Layout                  = True
  Match                   == Match                   = True
  Operator a              == Operator b              = a == b
  Permission              == Permission              = True
  Reference               == Reference               = True
  Return                  == Return                  = True
  Synonym                 == Synonym                 = True
  Text a                  == Text b                  = a == b
  Trait                   == Trait                   = True
  Type                    == Type                    = True
  VectorBegin             == VectorBegin             = True
  VectorEnd               == VectorEnd               = True
  Vocab                   == Vocab                   = True
  VocabLookup             == VocabLookup             = True
  Word a                  == Word b                  = a == b
  _                       == _                       = False

instance Pretty Token where
  pPrint token = case token of
    About -> "about"
    AngleBegin -> "<"
    AngleEnd -> ">"
    Arrow -> "->"
    BlockBegin{} -> "{"
    BlockEnd -> "}"
    Call -> "call"
    Case -> "case"
    Character c -> Pretty.quotes $ Pretty.char c
    Comma -> ","
    Define -> "define"
    Do -> "do"
    Ellipsis -> "..."
    Elif -> "elif"
    Else -> "else"
    Float a b c -> Pretty.double $ float a b c
    GroupBegin -> "("
    GroupEnd -> ")"
    If -> "if"
    Ignore -> "_"
    Infix -> "infix"
    Instance -> "instance"
    Integer value hint
      -> Pretty.text $ if value < 0 then '-' : shown else shown
      where
      shown = prefix ++ showIntAtBase base (digits !!) (abs value) ""
      (base, prefix, digits) = case hint of
        Binary -> (2, "0b", "01")
        Octal -> (8, "0o", ['0'..'7'])
        Decimal -> (10, "", ['0'..'9'])
        Hexadecimal -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
    Jump -> "jump"
    Layout -> ":"
    Match -> "match"
    Operator name -> pPrint name
    Permission -> "permission"
    Reference -> "\\"
    Return -> "return"
    Synonym -> "synonym"
    Text t -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t
    Trait -> "trait"
    Type -> "type"
    VectorBegin -> "["
    VectorEnd -> "]"
    Vocab -> "vocab"
    VocabLookup -> "::"
    Word name -> pPrint name

-- Minor hack because Parsec requires 'Show'.
instance Show Token where
  show = Pretty.render . pPrint

-- Note [Angle Brackets]:
--
-- Since we separate the passes of tokenization and parsing, we are faced with a
-- classic ambiguity between angle brackets as used in operator names such as
-- '>>' and '<+', and as used in type argument and parameter lists such as
-- 'vector<vector<T>>' and '<+E>'.
--
-- Our solution is to parse a less-than or greater-than character as an 'angle'
-- token if it was immediately followed by a symbol character in the input, with
-- no intervening whitespace. This is enough information for the parser to
-- disambiguate the intent:
--
--   • When parsing an expression, it joins a sequence of angle tokens and
--     an operator token into a single operator token.
--
--   • When parsing a signature, it treats them separately.
--
-- You may ask why we permit this silly ambiguity in the first place. Why not
-- merge the passes of tokenization and parsing, or use a different bracketing
-- character such as '[]' for type argument lists?
--
-- We separate tokenization and parsing for the sake of tool support: it's
-- simply easier to provide token-accurate source locations when we keep track
-- of source locations at the token level, and it's easier to provide a list of
-- tokens to external tools (e.g., for syntax highlighting) if we already have
-- such a list at hand.
--
-- The reason for the choice of bracketing character is for the sake of
-- compatibility with C++ tools. When setting a breakpoint in GDB, for example,
-- it's nice to be able to type:
--
--     break foo::bar<int>
--
-- And for this to refer to the Kitten definition 'foo::bar<int>' precisely,
-- rather than to some syntactic analogue such as 'foo.bar[int]'. The modest
-- increase in complexity of implementation is justified by fostering a better
-- experience for people.

-- Note [Float Literals]:
--
-- Floating-point literals are represented as a pair of an arbitrary-precision
-- integer significand and exponent, so that:
--
--     Float a b c
--
-- Denotes the floating point number (a × 10^(c - b)). This representation was
-- chosen to avoid loss of precision until the token is converted into a machine
-- floating-point format. The exponent is split into two parts to indicate which
-- part of the literal that exponent came from: the fractional part, or the
-- exponent in scientific notation.

float :: Fractional a => Integer -> Int -> Int -> a
float a b c = let
  e = c - b
  -- The intermediate rational step is necessary to preserve precision.
  shift = if e < 0 then 1 % 10 ^ negate e else 10 ^ e
  in fromRational $ (fromIntegral a :: Rational) * shift
