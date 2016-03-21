{-# LANGUAGE OverloadedStrings #-}

module Kitten.Bits
  ( FloatBits(..)
  , IntegerBits(..)
  ) where

import Text.PrettyPrint.HughesPJClass (Pretty(..))

data FloatBits
  = Float32
  | Float64
  deriving (Eq, Show)

data IntegerBits
  = Signed8
  | Signed16
  | Signed32
  | Signed64
  | Unsigned8
  | Unsigned16
  | Unsigned32
  | Unsigned64
  deriving (Eq, Show)

instance Pretty IntegerBits where
  pPrint bits = case bits of
    Signed8 -> "i8"
    Signed16 -> "i16"
    Signed32 -> "i32"
    Signed64 -> "i64"
    Unsigned8 -> "u8"
    Unsigned16 -> "u16"
    Unsigned32 -> "u32"
    Unsigned64 -> "u64"

instance Pretty FloatBits where
  pPrint bits = case bits of
    Float32 -> "f32"
    Float64 -> "f64"
