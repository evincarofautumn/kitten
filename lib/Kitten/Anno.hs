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
  Composition :: [Type Scalar] -> Type Row
  Vector :: Type Scalar -> Type Scalar
  Bool :: Type Scalar
  Float :: Type Scalar
  Int :: Type Scalar
  Text :: Type Scalar
  Any :: Type a

instance Eq (Type a) where
  Bool == Bool = True
  Float == Float = True
  Int == Int = True
  Text == Text = True
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  Vector a == Vector b = a == b
  Any == _ = True
  _ == Any = True
  _ == _ = False

instance Show (Type a) where
  show type_ = case type_ of
    a :> b -> concat ["(", show a, " -> ", show b, ")"]
    Composition as -> showWords as
    Vector a -> concat ["[", show a, "]"]
    Bool -> "bool"
    Float -> "float"
    Int -> "int"
    Text -> "text"
    Any -> "*"
