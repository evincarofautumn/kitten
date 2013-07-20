{-# LANGUAGE GADTs #-}
{-# LANGUAGE PostfixOperators #-}

module Kitten.Type
  ( module Kitten.Kind
  , Scheme(..)
  , Type(..)
  , TypeName(..)
  , (-->)
  , (==>)
  , (+:)
  , effect
  , mono
  , row
  , scalar
  ) where

import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Kind
import Kitten.Name

data Type a where
  (:&) :: Type Scalar -> Type Scalar -> Type Scalar
  (:.) :: Type Row -> Type Scalar -> Type Row
  (:?) :: Type Scalar -> Type Scalar
  (:|) :: Type Scalar -> Type Scalar -> Type Scalar
  Bool :: Type Scalar
  Char :: Type Scalar
  Empty :: Type Row
  Float :: Type Scalar
  Function :: Type Row -> Type Row -> Type Effect -> Type Scalar
  Handle :: Type Scalar
  Int :: Type Scalar
  Test :: Type a
  Unit :: Type Scalar
  Var :: Name -> Type a
  Vector :: Type Scalar -> Type Scalar

  (:+) :: Type Effect -> Type Effect -> Type Effect
  NoEffect :: Type Effect
  IOEffect :: Type Effect

newtype TypeName a = TypeName { typeName :: Name }
  deriving (Eq, Ord, Show)

effect :: Name -> TypeName Effect
effect = TypeName

row :: Name -> TypeName Row
row = TypeName

scalar :: Name -> TypeName Scalar
scalar = TypeName

infix 6 :&
infix 6 :|
infixl 5 :.
infix 4 -->
infix 4 ==>

(-->) :: Type Row -> Type Row -> Type Scalar
a --> b = Function a b NoEffect

(==>) :: Type Row -> Type Row -> Type Scalar
a ==> b = Function a b IOEffect

(+:) :: Type Effect -> Type Effect -> Type Effect
IOEffect +: IOEffect = IOEffect
IOEffect +: NoEffect = IOEffect
NoEffect +: IOEffect = IOEffect
NoEffect +: NoEffect = NoEffect
a +: b = a :+ b

data Scheme = Forall
  (Set (TypeName Row))
  (Set (TypeName Scalar))
  (Set (TypeName Effect))
  (Type Scalar)
  deriving (Eq, Show)

instance Eq (Type a) where
  Test == _ = True
  _ == Test = True

  (a :& b) == (c :& d) = (a, b) == (c, d)
  (a :. b) == (c :. d) = (a, b) == (c, d)
  (:?) a == (:?) b = a == b
  (a :| b) == (c :| d) = (a, b) == (c, d)
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

  (a :+ b) == (c :+ d) = (a, b) == (c, d)
  NoEffect == NoEffect = True
  IOEffect == IOEffect = True

  _ == _ = False

instance Show (Type a) where
  showsPrec _ type_ = case type_ of
    t1 :& t2 -> showParen True
      $ shows t1 . showString " & " . shows t2
    t1 :. t2 -> shows t1 . showChar ' ' . shows t2
    (:?) t -> shows t . showChar '?'
    t1 :| t2 -> showParen True
      $ shows t1 . showString " | " . shows t2
    Empty -> id
    Bool -> showString "Bool"
    Char -> showString "Char"
    Float -> showString "Float"
    Function r1 r2 e
      -> showParen True
      $ shows r1
      . showString " -> "
      . shows r2
      . showString " + "
      . shows e
    Handle -> showString "Handle"
    Int -> showString "Int"
    Test -> id
    Var (Name index) -> showChar 't' . shows index
    Unit -> showString "()"
    Vector t -> showChar '[' . shows t . showChar ']'

    t1 :+ t2 -> showParen True
      $ shows t1 . showString " + " . shows t2
    NoEffect -> showString "()"
    IOEffect -> showString "IO"

mono :: Type Scalar -> Scheme
mono = Forall Set.empty Set.empty Set.empty
