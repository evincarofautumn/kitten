{-|
Module      : Kitten.Literal
Description : Representations of literal values
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Literal
  ( FloatLiteral(..)
  , IntegerLiteral(..)
  , floatValue
  ) where

import Data.Ratio ((%))
import Kitten.Base (Base(..))
import Kitten.Bits (FloatBits(..), IntegerBits(..))
import Numeric
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

data IntegerLiteral = IntegerLiteral
  { integerValue :: !Integer
  , integerBase :: !Base
  , integerBits :: !IntegerBits
  } deriving (Show)

-- Integer literals compare equality regardless of base and bits.
instance Eq IntegerLiteral where
  IntegerLiteral a _baseA _bitsA == IntegerLiteral b _baseB _bitsB = a == b

instance Pretty IntegerLiteral where
  pPrint literal = Pretty.hcat
    [ if value < 0 then "-" else ""
    , prefix
    , Pretty.text $ showIntAtBase base (digits !!) (abs value) ""
    , suffix
    ]
    where
      value = integerValue literal
      bits = integerBits literal
      (base, prefix, digits) = case integerBase literal of
        Binary -> (2, "0b", "01")
        Octal -> (8, "0o", ['0'..'7'])
        Decimal -> (10, "", ['0'..'9'])
        Hexadecimal -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
      suffix = case bits of
        Signed32 -> ""
        _ -> pPrint bits

data FloatLiteral = FloatLiteral
  { floatSignificand :: !Integer
  , floatFractional :: !Int
  , floatExponent :: !Int
  , floatBits :: !FloatBits
  } deriving (Show)

-- Float literals compar equality regardless of bits.
instance Eq FloatLiteral where
  FloatLiteral a b c _bitsA == FloatLiteral d e f _bitsB = (a, c - b) == (d, f - e)

instance Pretty FloatLiteral where
  pPrint literal
    = Pretty.hcat
      [ if value < 0 then "-" else ""
      , Pretty.double value
      , case bits of
        Float64 -> ""
        Float32 -> pPrint bits
      ]
    where
      bits = floatBits literal
      value = floatValue literal

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

floatValue :: Fractional a => FloatLiteral -> a
floatValue (FloatLiteral a b c _bits) = let
  e = c - b
  -- The intermediate rational step is necessary to preserve precision.
  shift = if e < 0 then 1 % 10 ^ negate e else 10 ^ e
  in fromRational $ (fromIntegral a :: Rational) * shift
