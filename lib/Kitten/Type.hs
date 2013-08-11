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
import Kitten.Location
import Kitten.Name

data Type a where
  (:&) :: Type Scalar -> Type Scalar -> Type Scalar
  (:.) :: Type Row -> Type Scalar -> Type Row
  (:?) :: Type Scalar -> Type Scalar
  (:|) :: Type Scalar -> Type Scalar -> Type Scalar
  Bool :: Location -> Type Scalar
  Char :: Location -> Type Scalar
  Empty :: Location -> Type Row
  Float :: Location -> Type Scalar
  Function :: Type Row -> Type Row -> Type Effect -> Location -> Type Scalar
  Handle :: Location -> Type Scalar
  Int :: Location -> Type Scalar
  Named :: String -> Location -> Type Scalar
  Test :: Type a
  Unit :: Location -> Type Scalar
  Var :: Name -> Location -> Type a
  Vector :: Type Scalar -> Location -> Type Scalar

  (:+) :: Type Effect -> Type Effect -> Type Effect
  NoEffect :: Location -> Type Effect
  IOEffect :: Location -> Type Effect

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
a --> b = Function a b (NoEffect UnknownLocation) UnknownLocation

(==>) :: Type Row -> Type Row -> Type Scalar
a ==> b = Function a b (IOEffect UnknownLocation) UnknownLocation

(+:) :: Type Effect -> Type Effect -> Type Effect
a +: b | a == b = a
IOEffect loc +: _ = IOEffect loc
NoEffect _ +: a = a
_ +: IOEffect loc = IOEffect loc
a +: NoEffect _ = a
(a :+ b) +: c = a +: (b +: c)
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
  Bool{} == Bool{} = True
  Char{} == Char{} = True
  Empty{} == Empty{} = True
  Float{} == Float{} = True
  Function a b p1 _ == Function c d p2 _
    = (a, b, p1) == (c, d, p2)
  Handle{} == Handle{} = True
  Int{} == Int{} = True
  Named a _ == Named b _ = a == b
  Unit{} == Unit{} = True
  Var a _ == Var b _ = a == b
  Vector a _ == Vector b _ = a == b

  (a :+ b) == (c :+ d) = (a, b) == (c, d)
  NoEffect{} == NoEffect{} = True
  IOEffect{} == IOEffect{} = True

  _ == _ = False

instance Show (Type a) where
  showsPrec _ type_ = case type_ of
    t1 :& t2 -> showParen True
      $ shows t1 . showString " & " . shows t2
    t1 :. t2 -> shows t1 . showChar ' ' . shows t2
    (:?) t -> shows t . showChar '?'
    t1 :| t2 -> showParen True
      $ shows t1 . showString " | " . shows t2
    Empty{} -> showString "<empty>"
    Bool{} -> showString "Bool"
    Char{} -> showString "Char"
    Float{} -> showString "Float"
    Function r1 r2 e _
      -> showParen True
      $ shows r1
      . showString " -> "
      . shows r2
      . showString " + "
      . shows e
    Handle{} -> showString "Handle"
    Int{} -> showString "Int"
    Named name _ -> showString name
    Test{} -> id
    Var (Name index) _ -> showChar 't' . shows index
    Unit{} -> showString "()"
    Vector t _ -> showChar '[' . shows t . showChar ']'

    t1 :+ t2 -> showParen True
      $ shows t1 . showString " + " . shows t2
    NoEffect{} -> showString "()"
    IOEffect{} -> showString "IO"

mono :: Type Scalar -> Scheme
mono = Forall Set.empty Set.empty Set.empty
