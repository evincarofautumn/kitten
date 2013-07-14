{-# LANGUAGE GADTs #-}

module Kitten.Type
  ( module Kitten.Kind
  , Scheme(..)
  , Type(..)
  , TypeName(..)
  , (-->)
  , (==>)
  , isImpure
  , isPure
  , mono
  , row
  , scalar
  ) where

import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Kind
import Kitten.Name
import Kitten.Purity

data Type a where
  (:&) :: Type Scalar -> Type Scalar -> Type Scalar
  (:.) :: Type Row -> Type Scalar -> Type Row
  Bool :: Type Scalar
  Char :: Type Scalar
  Empty :: Type Row
  Float :: Type Scalar
  Function :: Type Row -> Type Row -> Purity -> Type Scalar
  Handle :: Type Scalar
  Int :: Type Scalar
  Test :: Type a
  Unit :: Type Scalar
  Var :: Name -> Type a
  Vector :: Type Scalar -> Type Scalar

newtype TypeName a = TypeName { typeName :: Name }
  deriving (Eq, Ord, Show)

row :: Name -> TypeName Row
row = TypeName

scalar :: Name -> TypeName Scalar
scalar = TypeName

infixr 6 :&
infixl 5 :.
infix 4 -->
infix 4 ==>

isImpure :: Type Scalar -> Bool
isImpure = not . isPure

isPure :: Type Scalar -> Bool
isPure (Function _ _ Impure) = False
isPure _ = True

(-->) :: Type Row -> Type Row -> Type Scalar
a --> b = Function a b Pure

(==>) :: Type Row -> Type Row -> Type Scalar
a ==> b = Function a b Impure

data Scheme = Forall
  (Set (TypeName Row))
  (Set (TypeName Scalar))
  (Type Scalar)
  deriving (Eq, Show)

instance Eq (Type a) where
  Test == _ = True
  _ == Test = True

  (a :& b) == (c :& d) = (a, b) == (c, d)
  Bool == Bool = True
  Char == Char = True
  Empty == Empty = True
  Float == Float = True
  Function a b p1 == Function c d p2 = (a, b, p1) == (c, d, p2)
  Handle == Handle = True
  Int == Int = True
  Unit == Unit = True
  Var a == Var b = a == b
  Vector a == Vector b = a == b
  _ == _ = False

instance Show (Type a) where
  showsPrec _ type_ = case type_ of
    t1 :& t2 -> showParen True
      $ shows t1 . showString " & " . shows t2
    t1 :. t2 -> shows t1 . showChar ' ' . shows t2
    Empty -> id
    Bool -> showString "Bool"
    Char -> showString "Char"
    Float -> showString "Float"
    Function r1 r2 purity
      -> showParen True
      $ shows r1
      . shows purity
      . shows r2
    Handle -> showString "Handle"
    Int -> showString "Int"
    Test -> id
    Var (Name index) -> showChar 't' . shows index
    Unit -> showString "()"
    Vector t -> showChar '[' . shows t . showChar ']'

mono :: Type Scalar -> Scheme
mono = Forall Set.empty Set.empty
