{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Kitten.Type
  ( Row
  , Scalar
  , Type(..)
  , instanceOf
  ) where

import Kitten.Name

data Scalar
data Row

data Type a where
  BoolType :: Type Scalar
  IntType :: Type Scalar
  TextType :: Type Scalar
  (:>) :: Type Row -> Type Row -> Type Scalar
  Composition :: [Type Scalar] -> Type Row
  VecType :: Type Scalar -> Type Scalar
  TupleType :: [Type Scalar] -> Type Scalar
  ScalarVar :: Name -> Type Scalar
  RowVar :: Name -> Type Row
  EmptyType :: Type Row

instance Eq (Type a) where
  BoolType == BoolType = True
  IntType == IntType = True
  TextType == TextType = True
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  VecType a == VecType b = a == b
  TupleType as == TupleType bs = as == bs
  ScalarVar a == ScalarVar b = a == b
  RowVar a == RowVar b = a == b
  EmptyType == EmptyType = True
  _ == _ = False

infix 4 :>

instance Show (Type a) where
  show IntType = "int"
  show BoolType = "bool"
  show TextType = "text"
  show (ScalarVar name) = show name
  show (RowVar name) = show name
  show (VecType type_)
    = "[" ++ show type_ ++ "]"
  show (TupleType types)
    = "(" ++ unwords (map show types) ++ ")"
  show (a :> b)
    = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Composition as) = unwords (map show as)
  show EmptyType = "()"

{-
class InstanceOf a where
  instanceOf :: a -> a -> Bool
-}

instanceOf :: Type Scalar -> Type Scalar -> Bool
instanceOf type1 type2
  | type1 == type2 = True
  | otherwise = case (type1, type2) of
    (ScalarVar a, ScalarVar b) -> a == b
    (_, ScalarVar _) -> True
    (a :> b, c :> d) -> instanceOfRow a c && instanceOfRow b d
    (VecType a, VecType b) -> a `instanceOf` b
    (TupleType as, TupleType bs) -> instancesOf as bs
    _ -> False

instanceOfRow :: Type Row -> Type Row -> Bool
instanceOfRow type1 type2
  | type1 == type2 = True
  | otherwise = case (type1, type2) of
    (RowVar a, RowVar b) -> a == b
    (_, RowVar _) -> True
    (Composition as, Composition bs) -> instancesOf as bs
    _ -> False

instancesOf :: [Type Scalar] -> [Type Scalar] -> Bool
instancesOf as bs
  = length as == length bs
  && all (uncurry instanceOf) (zip as bs)
