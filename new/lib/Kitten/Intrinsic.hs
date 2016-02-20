{-# LANGUAGE OverloadedStrings #-}

module Kitten.Intrinsic
  ( Intrinsic(..)
  ) where

import Text.PrettyPrint.HughesPJClass (Pretty(..))

data Intrinsic
  = AddInt
  | Magic
  deriving (Eq, Ord, Show)

instance Pretty Intrinsic where
  pPrint intrinsic = case intrinsic of
    AddInt -> ".add.int"
    Magic -> ".magic"
