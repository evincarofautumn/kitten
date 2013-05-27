{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

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
  (:>) :: Type Row -> Type Row -> Type Scalar
  AnyType :: Type a
  BoolType :: Type Scalar
  CharType :: Type Scalar
  Composition :: [Type Scalar] -> Type Row
  FloatType :: Type Scalar
  HandleType :: Type Scalar
  IntType :: Type Scalar
  PairType :: Type Scalar -> Type Scalar -> Type Scalar
  StackFrameType :: Type Scalar
  UnitType :: Type Scalar
  VectorType :: Type Scalar -> Type Scalar

instance Eq (Type a) where
  (a :> b) == (c :> d) = (a, b) == (c, d)
  AnyType == _ = True
  _ == AnyType = True
  _ == (AnyType :> AnyType) = True  -- HACK
  (AnyType :> AnyType) == _ = True  -- HACK
  BoolType == BoolType = True
  Composition as == Composition bs = as == bs
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  CharType == CharType = True
  FloatType == FloatType = True
  HandleType == HandleType = True
  IntType == IntType = True
  PairType a b == PairType c d = (a, b) == (c, d)
  StackFrameType == StackFrameType = True
  UnitType == UnitType = True
  VectorType a == VectorType b = a == b
  _ == _ = False

infix 4 :>

instance Show (Type a) where
  show type_ = case type_ of
    AnyType -> "*"
    BoolType -> "Bool"
    CharType -> "Char"
    Composition [] :> a -> show a
    Composition as -> showWords as
    a :> b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
    FloatType -> "Float"
    HandleType -> "Handle"
    IntType -> "Int"
    PairType a b -> concat ["(", show a, ", ", show b, ")"]
    StackFrameType -> "empty stack"
    UnitType -> "()"
    VectorType a -> "[" ++ show a ++ "]"

fromAnno :: Anno -> Type Scalar
fromAnno (Anno type_ _) = fromAnnoType type_

fromAnnoType :: Anno.Type Scalar -> Type Scalar
fromAnnoType annoType = case annoType of
  a Anno.:> b -> fromAnnoRow a :> fromAnnoRow b
  Anno.Any -> AnyType
  Anno.Bool -> BoolType
  Anno.Char -> CharType
  Anno.Float -> FloatType
  Anno.Handle -> HandleType
  Anno.Int -> IntType
  Anno.Tuple types -> foldr (PairType . fromAnnoType) AnyType types
  Anno.Unit -> UnitType
  Anno.Vector type_ -> VectorType $ fromAnnoType type_

fromAnnoRow :: Anno.Type Row -> Type Row
fromAnnoRow anno = case anno of
  Anno.Composition types -> Composition $ map fromAnnoType types
  Anno.Any -> AnyType
