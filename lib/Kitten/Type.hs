{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Type
  ( Row
  , Scalar
  , Scheme(..)
  , Type(..)
  , fromAnno
  ) where

import Data.Set (Set)

import Kitten.Anno (Anno(..))
import Kitten.Kind
import Kitten.Name

import qualified Kitten.Anno as Anno

data Type a where
  BoolType :: Type Scalar
  IntType :: Type Scalar
  TextType :: Type Scalar
  (:>) :: Type Row -> Type Row -> Type Scalar
  Composition :: [Type Scalar] -> Type Row
  VectorType :: Type Scalar -> Type Scalar
  ScalarVar :: Name -> Type Scalar
  RowVar :: Name -> Type Row
  EmptyType :: Type Row

instance Eq (Type a) where
  BoolType == BoolType = True
  IntType == IntType = True
  TextType == TextType = True
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  VectorType a == VectorType b = a == b
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
  show (VectorType type_)
    = "[" ++ show type_ ++ "]"
  show (a :> b)
    = "{" ++ show a ++ " -> " ++ show b ++ "}"
  show (Composition as) = unwords (map show as)
  show EmptyType = "()"

data Scheme a = Forall (Set Name) (Type a)

fromAnno :: Anno -> Scheme Scalar
fromAnno Anno{..} = Forall annoVars $ fromAnnoType annoType

fromAnnoType :: Anno.Type Scalar -> Type Scalar
fromAnnoType annoType = case annoType of
  a Anno.:> b -> fromAnnoRow a :> fromAnnoRow b
  Anno.Vector type_ -> VectorType $ fromAnnoType type_
  Anno.Var name -> ScalarVar name
  Anno.Bool -> BoolType
  Anno.Int -> IntType
  Anno.Text -> TextType

fromAnnoRow :: Anno.Type Row -> Type Row
fromAnnoRow anno = case anno of
  Anno.Composition types -> Composition
    $ map fromAnnoType types
