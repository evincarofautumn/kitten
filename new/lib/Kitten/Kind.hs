{-# LANGUAGE OverloadedStrings #-}

module Kitten.Kind
  ( Kind(..)
  ) where

import Data.Hashable (Hashable(..))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

-- A kind (κ) is the type of a type. Types with the "value" kind (*) are
-- inhabited by values; all other types are used only to enforce program
-- invariants. These include:
--
--  • The "stack" kind (ρ), used to enforce that the stack cannot contain
--    other stacks.
--
--  • The "effect label" kind (λ), used to identify a side effect.
--
--  • The "effect" kind (ε), denoting a set of side effects.
--
--  • The "function" kind (κ → κ), used to describe type constructors.

data Kind = Value | Stack | Label | Effect | !Kind :-> !Kind
  deriving (Eq, Show)

instance Hashable Kind where
  hashWithSalt s kind = case kind of
    Value -> hashWithSalt s (0 :: Int)
    Stack -> hashWithSalt s (1 :: Int)
    Label -> hashWithSalt s (2 :: Int)
    Effect -> hashWithSalt s (3 :: Int)
    a :-> b -> hashWithSalt s (4 :: Int, a, b)

instance Pretty Kind where
  pPrint kind = case kind of
    Value -> "value"
    Stack -> "stack"
    Label -> "label"
    Effect -> "effect"
    a :-> b -> Pretty.parens $ Pretty.hsep
      [pPrint a, "->", pPrint b]
