{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type
  ( Constructor(..)
  , Type(..)
  , TypeId(..)
  , Var(..)
  , bottom
  , fun
  , join
  , prod
  , Kitten.Type.sum
  , void
  , setOrigin
  , origin
  ) where

import Data.Hashable (Hashable(..))
import GHC.Exts (IsString(..))
import Kitten.Kind (Kind(..))
import Kitten.Name (Qualified(..), Unqualified(..))
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

-- This is the type language. It describes a system of conventional Hindley–
-- Milner types, with type constructors joined by type application, as well as
-- type variables and constants for constraint solving and instance checking,
-- respectively. It syntactically permits higher-ranked quantification, though
-- there are semantic restrictions on this, discussed in the presentation of the
-- inference algorithm. Type variables have explicit kinds.

data Type
  = !Type :@ !Type
  | TypeConstructor !Origin !Constructor
  | TypeVar !Origin !Var
  | TypeConstant !Origin !Var
  | Forall !Origin !Var !Type
  | TypeValue !Origin !Int
 deriving (Show)

infixl 1 :@

newtype Constructor = Constructor Qualified
  deriving (Eq, Hashable, Show)

data Var = Var !TypeId !Kind
  deriving (Eq, Show)

bottom :: Origin -> Type
bottom o = TypeConstructor o "Bottom"

fun :: Origin -> Type -> Type -> Type -> Type
fun o a b e = TypeConstructor o "Fun" :@ a :@ b :@ e

prod :: Origin -> Type -> Type -> Type
prod o a b = TypeConstructor o "Prod" :@ a :@ b

sum :: Origin -> Type -> Type -> Type
sum o a b = TypeConstructor o "Sum" :@ a :@ b

join :: Origin -> Type -> Type -> Type
join o a b = TypeConstructor o "Join" :@ a :@ b

void :: Origin -> Type
void o = TypeConstructor o "Void"

origin :: Type -> Origin
origin type_ = case type_ of
  a :@ _ -> origin a
  TypeConstructor o _ -> o
  TypeVar o _ -> o
  TypeConstant o _ -> o
  Forall o _ _ -> o
  TypeValue o _ -> o

setOrigin :: Origin -> Type -> Type
setOrigin o = go
  where
  go type_ = case type_ of
    a :@ b -> go a :@ go b
    TypeConstructor _ constructor -> TypeConstructor o constructor
    TypeVar _ var -> TypeVar o var
    TypeConstant _ var -> TypeConstant o var
    Forall _ var t -> Forall o var $ go t
    TypeValue _ x -> TypeValue o x

-- Type variables are distinguished by globally unique identifiers. This makes
-- it easier to support capture-avoiding substitution on types.

newtype TypeId = TypeId Int
  deriving (Enum, Eq, Hashable, Ord, Show)

instance Eq Type where
  (a :@ b) == (c :@ d) = (a, b) == (c, d)
  TypeConstructor _ a == TypeConstructor _ b = a == b
  TypeVar _ a == TypeVar _ b = a == b
  TypeConstant _ a == TypeConstant _ b = a == b
  Forall _ a b == Forall _ c d = (a, b) == (c, d)
  _ == _ = False

instance Hashable Type where
  hashWithSalt s type_ = case type_ of
    a :@ b -> hashWithSalt s (0 :: Int, a, b)
    TypeConstructor _ a -> hashWithSalt s (1 :: Int, a)
    TypeVar _ a -> hashWithSalt s (2 :: Int, a)
    TypeConstant _ a -> hashWithSalt s (3 :: Int, a)
    Forall _ a b -> hashWithSalt s (4 :: Int, a, b)
    TypeValue _ a -> hashWithSalt s (5 :: Int, a)

instance Hashable Var where
  hashWithSalt s (Var a b) = hashWithSalt s (0 :: Int, a, b)

instance IsString Constructor where
  fromString = Constructor
    . Qualified Vocabulary.global . Unqualified . Text.pack

instance Pretty Constructor where
  pPrint (Constructor name) = pPrint name

instance Pretty Type where
  pPrint type_ = case type_ of
    TypeConstructor _ "Fun" :@ a :@ b :@ e -> Pretty.parens
      $ Pretty.hsep [pPrint a, "->", pPrint b, pPrint e]
    TypeConstructor _ "Prod" :@ a :@ b
      -> Pretty.hcat [pPrint a, ", ", pPrint b]
    TypeConstructor _ "Sum" :@ a :@ b
      -> Pretty.hcat [pPrint a, " | ", pPrint b]
    TypeConstructor _ "Join" :@ a :@ b
      -> Pretty.hcat ["+", pPrint a, " ", pPrint b]
    a :@ b -> Pretty.hcat [pPrint a, Pretty.angles $ pPrint b]
    TypeConstructor _ constructor -> pPrint constructor
    TypeVar _ var -> pPrint var
    TypeConstant _ var -> pPrint var
    Forall{} -> prettyForall type_ []
      where
      prettyForall (Forall _ x t) vars = prettyForall t (x : vars)
      prettyForall t vars = Pretty.hcat
        [ Pretty.angles $ Pretty.list $ map pPrint vars
        , Pretty.parens $ pPrint t
        ]
    TypeValue _ value -> Pretty.int value

instance Pretty TypeId where
  pPrint (TypeId i) = Pretty.hcat [Pretty.char 'T', Pretty.int i]

instance Pretty Var where
  pPrint (Var i kind) = Pretty.hcat $ case kind of
    Permission -> ["+", v]
    Stack -> [v, "..."]
    _ -> [v]
    where
    v = pPrint i
