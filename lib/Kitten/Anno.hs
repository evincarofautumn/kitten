{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Kitten.Location
import Kitten.Purity
import Kitten.Util.Show

data Anno = Anno Type Location
  deriving (Eq)

instance Show Anno where
  show (Anno type_ _) = show type_

data Type
  = Function [Type] [Type] Purity
  | Bool
  | Char
  | Choice Type Type
  | Float
  | Handle
  | Int
  | Option Type
  | Pair Type Type
  | Unit
  | Var String
  | Vector Type
  deriving (Eq)

instance Show Type where
  show type_ = case type_ of
    Function a b purity -> concat
      [ "("
      , showWords a
      , show purity
      , showWords b
      , ")"
      ]
    Bool -> "Bool"
    Char -> "Char"
    Choice a b -> concat
      [ "("
      , show a
      , " | "
      , show b
      , ")"
      ]
    Float -> "Float"
    Handle -> "Handle"
    Int -> "Int"
    Option a -> show a ++ "?"
    Pair a b -> concat ["(", show a, " & ", show b, ")"]
    Unit -> "()"
    Var a -> a
    Vector a -> concat ["[", show a, "]"]
