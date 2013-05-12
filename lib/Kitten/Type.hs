{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Type
  ( Row
  , Scalar
  , Type(..)
  , fromAnno
  ) where

import Kitten.Anno (Anno(..))
import Kitten.Kind
import Kitten.Util.Show

import qualified Kitten.Anno as Anno

data Type a where
  BoolType :: Type Scalar
  CharType :: Type Scalar
  FloatType :: Type Scalar
  IntType :: Type Scalar
  UnitType :: Type Scalar
  (:>) :: Type Row -> Type Row -> Type Scalar
  Composition :: [Type Scalar] -> Type Row
  VectorType :: Type Scalar -> Type Scalar
  PairType :: Type Scalar -> Type Scalar -> Type Scalar
  AnyType :: Type a
  StackFrameType :: Type Scalar

instance Eq (Type a) where
  BoolType == BoolType = True
  CharType == CharType = True
  FloatType == FloatType = True
  IntType == IntType = True
  UnitType == UnitType = True
  _ == (AnyType :> AnyType) = True  -- HACK
  (AnyType :> AnyType) == _ = True  -- HACK
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  (a :> b) == (c :> d) = (a, b) == (c, d)
  Composition as == Composition bs = as == bs
  VectorType a == VectorType b = a == b
  PairType a b == PairType c d = (a, b) == (c, d)
  StackFrameType == StackFrameType = True
  AnyType == _ = True
  _ == AnyType = True
  _ == _ = False

infix 4 :>

instance Show (Type a) where
  show type_ = case type_ of
    FloatType -> "Float"
    IntType -> "Int"
    BoolType -> "Bool"
    CharType -> "Char"
    UnitType -> "()"
    VectorType a -> "[" ++ show a ++ "]"
    PairType a b -> concat ["(", show a, ", ", show b, ")"]
    Composition [] :> a -> show a
    a :> b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
    Composition as -> showWords as
    AnyType -> "*"
    StackFrameType -> "empty stack"

fromAnno :: Anno -> Type Scalar
fromAnno (Anno type_ _) = fromAnnoType type_

fromAnnoType :: Anno.Type Scalar -> Type Scalar
fromAnnoType annoType = case annoType of
  a Anno.:> b -> fromAnnoRow a :> fromAnnoRow b
  Anno.Vector type_ -> VectorType $ fromAnnoType type_
  Anno.Tuple types
    -> foldr PairType AnyType (map fromAnnoType types)
  Anno.Bool -> BoolType
  Anno.Char -> CharType
  Anno.Float -> FloatType
  Anno.Int -> IntType
  Anno.Unit -> UnitType
  Anno.Any -> AnyType

fromAnnoRow :: Anno.Type Row -> Type Row
fromAnnoRow anno = case anno of
  Anno.Composition types -> Composition
    $ map fromAnnoType types
  Anno.Any -> AnyType
