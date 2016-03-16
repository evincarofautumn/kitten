{-# LANGUAGE OverloadedStrings #-}

module Kitten.Entry.Parameter
  ( Parameter(..)
  ) where

import Kitten.Kind (Kind(..))
import Kitten.Name (Unqualified)
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

-- A generic type parameter for a data type.
data Parameter = Parameter !Origin !Unqualified !Kind
  deriving (Show)

-- Parameters are compared regardless of origin.
instance Eq Parameter where
  Parameter _ a b == Parameter _ c d = (a, b) == (c, d)

instance Pretty Parameter where
  pPrint (Parameter _ name kind) = case kind of
    Value -> pPrint name
    Stack -> Pretty.hcat [pPrint name, "..."]
    Label -> Pretty.hcat ["+", pPrint name]
    Permission -> Pretty.hcat ["+", pPrint name]
    _ :-> _ -> Pretty.hcat [pPrint name, "<_>"]
