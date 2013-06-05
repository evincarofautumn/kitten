module Kitten.Type
  ( Scheme(..)
  , Type(..)
  , mono
  ) where

import Data.List
import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Name
import Kitten.Util.Show

-- | τ
data Type

  -- | (τ × τ) :: τ
  = Type :* Type

  -- | τ* → τ* :: τ
  | [Type] :> [Type]

  -- | Bool :: τ
  | BoolType

  -- | Char :: τ
  | CharType

  -- | Float :: τ
  | FloatType

  | GeneratedType

  -- | Handle :: τ
  | HandleType

  -- | Int :: τ
  | IntType

  | TestType

  -- | a
  | TypeVar Name

  -- | () :: τ
  | UnitType

  -- | [τ] :: τ
  | VectorType Type

infixr 5 :*
infix 4 :>

-- | σ
data Scheme
  -- | ∀(a, b, …). τ
  = Forall (Set Name) (Type)
  deriving (Eq)

instance Eq Type where
  TestType == _ = True
  _ == TestType = True
  GeneratedType == GeneratedType = True

  (a :* b) == (c :* d) = (a, b) == (c, d)
  (a :> b) == (c :> d) = (a, b) == (c, d)
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
    t1 :* t2 -> shows t1 . showString ", " . shows t2
    r1 :> r2
      -> showParen True
      $ showString (showWords r1)
      . showString " -> "
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
