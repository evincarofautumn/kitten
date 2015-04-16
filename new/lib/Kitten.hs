{-# LANGUAGE OverloadedStrings #-}

module Kitten
  ( Expr(..)
  , Name(..)
  , Program(..)
  , Type(..)
  , emptyProgram
  , programToC
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Map as Map

newtype Name = Name { nameComponents :: Vector Text }

newtype Program = Program { programDefs :: Map Name (Type, Expr) }

data Type = Type
data Expr = Expr



emptyProgram :: Program
emptyProgram = Program Map.empty

programToC :: Program -> Text
programToC _program = "butts"
