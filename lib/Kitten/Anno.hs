{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Kitten.Location
import Kitten.Util.Show

data Anno = Anno Type Location
  deriving (Eq)

instance Show Anno where
  show (Anno type_ _) = show type_

data Type
  = [Type] :> [Type]
  | Bool
  | Char
  | Float
  | Handle
  | Int
  | Pair Type Type
  | Unit
  | Var String
  | Vector Type
  deriving (Eq)

instance Show Type where
  show type_ = case type_ of
    a :> b -> concat
      [ "("
      , showWords a
      , " -> "
      , showWords b
      , ")"
      ]

    Bool -> "Bool"
    Char -> "Char"
    Float -> "Float"
    Handle -> "Handle"
    Int -> "Int"
    Pair a b -> concat ["(", show a, " & ", show b, ")"]
    Unit -> "()"
    Var a -> a
    Vector a -> concat ["[", show a, "]"]
