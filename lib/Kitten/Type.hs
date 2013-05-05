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

import qualified Kitten.Anno as Anno

data Type a where
  BoolType :: Type Scalar
  IntType :: Type Scalar
  TextType :: Type Scalar
  (:>) :: Type Row -> Type Row -> Type Scalar
  Composition :: [Type Scalar] -> Type Row
  VectorType :: Type Scalar -> Type Scalar
  AnyType :: Type a
  StackFrameType :: Type Scalar

instance Eq (Type a) where
  BoolType == BoolType = True
  IntType == IntType = True
  TextType == TextType = True
  _ == (AnyType :> AnyType) = True  -- HACK
  (AnyType :> AnyType) == _ = True  -- HACK
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  VectorType a == VectorType b = a == b
  StackFrameType == StackFrameType = True
  AnyType == _ = True
  _ == AnyType = True
  _ == _ = False

infix 4 :>

instance Show (Type a) where
  show type_ = case type_ of
    IntType -> "int"
    BoolType -> "bool"
    TextType -> "text"
    VectorType a -> "[" ++ show a ++ "]"
    Composition [] :> a -> show a
    a :> b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
    Composition as -> unwords (map show as)
    AnyType -> "*"
    StackFrameType -> "empty stack"

fromAnno :: Anno -> Type Scalar
fromAnno (Anno type_ _) = fromAnnoType type_

fromAnnoType :: Anno.Type Scalar -> Type Scalar
fromAnnoType annoType = case annoType of
  a Anno.:> b -> fromAnnoRow a :> fromAnnoRow b
  Anno.Vector type_ -> VectorType $ fromAnnoType type_
  Anno.Bool -> BoolType
  Anno.Int -> IntType
  Anno.Text -> TextType
  Anno.Any -> AnyType

fromAnnoRow :: Anno.Type Row -> Type Row
fromAnnoRow anno = case anno of
  Anno.Composition types -> Composition
    $ map fromAnnoType types
  Anno.Any -> AnyType
