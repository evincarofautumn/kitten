{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Data.Set (Set)

import Kitten.Kind
import Kitten.Name

data Anno = Anno
  { annoName :: String
  , annoVars :: Set Name
  , annoType :: Type Scalar
  } deriving (Eq, Show)

data Type a where
  (:>) :: Type Row -> Type Row -> Type Scalar
  Composition :: [Type Scalar] -> Type Row
  Vec :: Type Scalar -> Type Scalar
  Tuple :: [Type Scalar] -> Type Scalar
  Var :: Name -> Type Scalar
  Bool :: Type Scalar
  Int :: Type Scalar
  Text :: Type Scalar

instance Eq (Type a) where
  Bool == Bool = True
  Int == Int = True
  Text == Text = True
  (a :> b) == (c :> d) = a == c && b == d
  Composition as == Composition bs = as == bs
  Vec a == Vec b = a == b
  Tuple as == Tuple bs = as == bs
  Var a == Var b = a == b
  _ == _ = False

instance Show (Type a) where
  show _ = "(type signature)"
