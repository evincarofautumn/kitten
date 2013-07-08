module Kitten.Type
  ( Scheme(..)
  , Type(..)
  , (-->)
  , (==>)
  , isImpure
  , isPure
  , mono
  ) where

import Data.List
import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Name
import Kitten.Purity
import Kitten.Util.Show

data Type
  = Type :& Type
  | FunctionType [Type] [Type] Purity
  | BoolType
  | CharType
  | FloatType
  | GeneratedType
  | HandleType
  | IntType
  | TestType
  | TypeVar Name
  | UnitType
  | VectorType Type

infixr 5 :&
infix 4 -->
infix 4 ==>

(-->) :: [Type] -> [Type] -> Type
a --> b = FunctionType a b Pure

(==>) :: [Type] -> [Type] -> Type
a ==> b = FunctionType a b Impure

isImpure :: Type -> Bool
isImpure = not . isPure

isPure :: Type -> Bool
isPure (FunctionType _ _ Impure) = False
isPure _ = True

data Scheme
  = Forall (Set Name) (Type)
  deriving (Eq)

instance Eq Type where
  TestType == _ = True
  _ == TestType = True
  GeneratedType == GeneratedType = True

  (a :& b) == (c :& d) = (a, b) == (c, d)
  FunctionType a b p1 == FunctionType c d p2 = (a, b, p1) == (c, d, p2)
  BoolType == BoolType = True
  CharType == CharType = True
  FloatType == FloatType = True
  HandleType == HandleType = True
  IntType == IntType = True
  TypeVar a == TypeVar b = a == b
  UnitType == UnitType = True
  VectorType a == VectorType b = a == b
  _ == _ = False

instance Show Type where
  showsPrec _ type_ = case type_ of
    t1 :& t2 -> showParen True
      $ shows t1 . showString " & " . shows t2
    FunctionType r1 r2 purity
      -> showParen True
      $ showString (showWords r1)
      . shows purity
      . showString (showWords r2)
    BoolType -> showString "Bool"
    CharType -> showString "Char"
    FloatType -> showString "Float"
    GeneratedType -> id
    HandleType -> showString "Handle"
    IntType -> showString "Int"
    TestType -> id
    TypeVar (Name index) -> showChar 't' . shows index
    UnitType -> showString "()"
    VectorType t -> showChar '[' . shows t . showChar ']'

instance Show Scheme where
  showsPrec precedence (Forall names type_) = let
    withQuantifier f | Set.null names = f
    withQuantifier f = showParen (precedence > 5)
      $ showChar '@'
      . showString
        (intercalate ", " . map show $ Set.toList names)
      . showString ". "
      . f
    in withQuantifier $ showsPrec precedence type_

mono :: Type -> Scheme
mono = Forall Set.empty
