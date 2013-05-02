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

instance Eq (Type a) where
  BoolType == BoolType = True
  IntType == IntType = True
  TextType == TextType = True
  a == (Composition [] :> Composition [b]) = a == b
  (Composition [] :> Composition [a]) == b = a == b
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  VectorType a == VectorType b = a == b
  AnyType == _ = True
  _ == AnyType = True
  _ == _ = False

infix 4 :>

instance Show (Type a) where
  show IntType = "int"
  show BoolType = "bool"
  show TextType = "text"
  show (VectorType type_)
    = "[" ++ show type_ ++ "]"
  show (Composition [] :> a)
    = show a
  show (a :> b)
    = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Composition as) = unwords (map show as)
  show AnyType = "*"

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
