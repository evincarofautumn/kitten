{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Kitten.Kind
import Kitten.Location
import Kitten.Util.Show

data Anno = Anno (Type Scalar) Location
  deriving (Eq)

instance Show Anno where
  show (Anno type_ _) = show type_

data Type a where
  (:>) :: Type Row -> Type Row -> Type Scalar
  Any :: Type a
  Bool :: Type Scalar
  Char :: Type Scalar
  Composition :: [Type Scalar] -> Type Row
  Float :: Type Scalar
  Handle :: Type Scalar
  Int :: Type Scalar
  Tuple :: [Type Scalar] -> Type Scalar
  Unit :: Type Scalar
  Vector :: Type Scalar -> Type Scalar

instance Eq (Type a) where
  Any == _ = True
  _ == Any = True
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  Bool == Bool = True
  Char == Char = True
  Float == Float = True
  Handle == Handle = True
  Int == Int = True
  Tuple as == Tuple bs = as == bs
  Unit == Unit = True
  Vector a == Vector b = a == b
  _ == _ = False

instance Show (Type a) where
  show type_ = case type_ of
    a :> b -> concat ["(", show a, " -> ", show b, ")"]
    Any -> "*"
    Bool -> "Bool"
    Char -> "Char"
    Composition as -> showWords as
    Float -> "Float"
    Handle -> "Handle"
    Int -> "Int"
    Tuple as -> concat ["[", showWords as, "]"]
    Unit -> "()"
    Vector a -> concat ["[", show a, "]"]
