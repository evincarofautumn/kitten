{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Kitten.Type
  ( module Kitten.Kind
  , Annotated(..)
  , Ctor(..)
  , Hint(..)
  , Origin(..)
  , Scheme(..)
  , StackHint(..)
  , Type(..)
  , TypeId(..)
  , TypeScheme
  , (-->)
  , addHint
  , bool
  , bottommost
  , char
  , float
  , handle
  , int
  , mono
  , scalar
  , stack
  , unScheme
  ) where

import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import qualified Data.Set as S

import Kitten.Builtin (Builtin)
import Kitten.Id
import Kitten.Kind
import Kitten.Location
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Util.Text as T

data Type (a :: Kind) where
  (:&) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  (:.) :: !(Type Stack) -> !(Type Scalar) -> Type Stack
  (:?) :: !(Type Scalar) -> Type Scalar
  (:|) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  Const :: !(TypeId a) -> !Origin -> Type a
  Ctor :: !Ctor -> !Origin -> Type Scalar
  Empty :: !Origin -> Type Stack
  Function :: !(Type Stack) -> !(Type Stack) -> !Origin -> Type Scalar
  Quantified :: !TypeScheme -> !Origin -> Type Scalar
  Var :: !(TypeId a) -> !Origin -> Type a
  Vector :: !(Type Scalar) -> !Origin -> Type Scalar

data Ctor
  = Bool
  | Char
  | Float
  | Handle
  | Int
  deriving (Eq)

bool, char, float, handle, int
  :: Origin -> Type Scalar
bool = Ctor Bool
char = Ctor Char
float = Ctor Float
handle = Ctor Handle
int = Ctor Int

instance ToText Ctor where
  toText = \case
    Bool -> "bool"
    Char -> "char"
    Float -> "float"
    Handle -> "handle"
    Int -> "int"

instance Show Ctor where
  show = T.unpack . toText

instance Eq (Type a) where
  (a :& b) == (c :& d) = (a, b) == (c, d)
  (a :. b) == (c :. d) = (a, b) == (c, d)
  (:?) a == (:?) b = a == b
  (a :| b) == (c :| d) = (a, b) == (c, d)
  Const a _ == Const b _ = a == b
  Ctor a _ == Ctor b _ = a == b
  Empty{} == Empty{} = True
  Function a b _ == Function c d _ = (a, b) == (c, d)
  Quantified a _ == Quantified b _ = a == b
  Var a _ == Var b _ = a == b
  Vector a _ == Vector b _ = a == b
  _ == _ = False

instance Show (Type Scalar) where
  show = T.unpack . toText

instance Show (Type Stack) where
  show = T.unpack . toText

-- TODO showsPrec
instance ToText (Type Scalar) where
  toText = \case
    t1 :& t2 -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    (:?) t -> toText t <> "?"
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    Const (TypeId (Id i)) o
      -> "t" <> showText i <> suffix o  -- TODO Show differently?
    Ctor name o -> toText name <> suffix o
    Function r1 r2 o -> T.concat
      [ "(", T.unwords [toText r1, "->", toText r2], ")"
      , suffix o
      ]
    Quantified scheme _ -> toText scheme
    Var (TypeId (Id i)) o
      -> "t" <> showText i <> suffix o
    Vector t o -> T.concat ["[", toText t, "]", suffix o]

instance ToText (Type Stack) where
  toText = \case
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    Const (TypeId (Id i)) o
      -> ".s" <> showText i <> suffix o  -- TODO Show differently?
    Empty o -> "<empty>" <> suffix o
    Var (TypeId (Id i)) o
      -> ".s" <> showText i <> suffix o

suffix :: Origin -> Text
suffix (Origin hint _) = case hint of
  Local name -> " (type of " <> name <> ")"
  AnnoType annotated
    -> " (type of " <> toText annotated <> ")"
  AnnoVar _ annotated
    -> " (from " <> toText annotated <> ")"
  AnnoFunctionInput annotated
    -> " (input to " <> toText annotated <> ")"
  AnnoFunctionOutput annotated
    -> " (output of " <> toText annotated <> ")"
  NoHint -> ""

newtype TypeId (a :: Kind) = TypeId { unTypeId :: Id TypeSpace }
  deriving (Eq, Ord)

instance Show (TypeId a) where
  show = T.unpack . toText

instance ToText (TypeId a) where
  toText = toText . unTypeId

data Annotated
  = AnnotatedDef !Text
  -- FIXME(strager): We can't use 'TypedDef', as that causes
  -- a circular dependency between Kitten.Type and
  -- Kitten.Typed.
  | Builtin !Builtin

instance Show Annotated where
  show = T.unpack . toText

instance ToText Annotated where
  toText (AnnotatedDef defName) = defName
  toText (Builtin builtin) = toText builtin

data Origin = Origin !Hint !Location

data Hint
  = Local !Text
  -- ^ Name introduced by a 'Scoped' term.

  | AnnoType !Annotated
  -- ^ Explicit type annotation.

  | AnnoVar !Text !Annotated
  -- ^ Type variable in a type annotation.

  | AnnoFunctionInput !Annotated
  | AnnoFunctionOutput !Annotated

  | NoHint

-- | Picks the most helpful hint.  'mappend' is commutative.
instance Monoid Hint where
  mempty = NoHint

  -- Note: patterns are ordered by preference.
  x@Local{} `mappend` _ = x
  _ `mappend` x@Local{} = x
  x@AnnoType{} `mappend` _ = x
  _ `mappend` x@AnnoType{} = x
  x@AnnoVar{} `mappend` _ = x
  _ `mappend` x@AnnoVar{} = x
  x@AnnoFunctionInput{} `mappend` _ = x
  _ `mappend` x@AnnoFunctionInput{} = x
  x@AnnoFunctionOutput{} `mappend` _ = x
  _ `mappend` x@AnnoFunctionOutput{} = x
  x@NoHint `mappend` _ = x

data StackHint
  = StackAny
  | Stack0
  | Stack1

-- FIXME Derived 'Eq' instance may be too restrictive.
data Scheme a = Forall
  (Set (TypeId Stack))
  (Set (TypeId Scalar))
  a
  deriving (Eq, Foldable, Functor, Traversable)

instance (ToText a) => Show (Scheme a) where
  show = T.unpack . toText

instance (ToText a) => ToText (Scheme a) where
  toText (Forall stacks scalars type_) = T.unwords
    $ (if null variables then id else (("forall" : variables ++ ["."]) ++))
    [toText type_]

    where
    variables :: [Text]
    variables = wordSetText stacks ++ wordSetText scalars

    wordSetText :: Set (TypeId a) -> [Text]
    wordSetText = map toText . S.toList

type TypeScheme = Scheme (Type Scalar)

infix 6 :&
infix 6 :|
infixl 5 :.
infix 4 -->

-- | Creates a 'Function' scalar type, inferring hints from
-- the given 'Origin'.
hintedFunction :: Type Stack -> Type Stack -> Origin -> Type Scalar
hintedFunction inputs outputs origin
  = Function inputs' outputs' origin
  where
  inputs', outputs' :: Type Stack
  (inputs', outputs') = case origin of
    Origin (AnnoType anno) _
      ->
        ( inputs `addStackHint` AnnoFunctionInput anno
        , outputs `addStackHint` AnnoFunctionOutput anno
        )
    _ -> (inputs, outputs)

(-->) :: Type Stack -> Type Stack -> Origin -> Type Scalar
(a --> b) origin = hintedFunction a b origin

addHint :: Type a -> Hint -> Type a
addHint type_ hint = case type_ of
  _ :& _ -> type_
  _ :. _ -> type_
  (:?) _ -> type_
  _ :| _ -> type_
  Empty o -> Empty (f o)
  Const i o -> Const i (f o)
  Ctor name o -> Ctor name (f o)
  Function r1 r2 o -> Function r1 r2 (f o)
  Quantified scheme o -> Quantified scheme o
  Var i o -> Var i (f o)
  Vector t o -> Vector t (f o)
  where
  f :: Origin -> Origin
  f (Origin _hint loc) = Origin hint loc

-- | Adds a 'Hint' to each 'Type Scalar' along a 'Type Stack'.
-- Shallow in scalars, deep in stacks.
--
-- TODO(strager): mapStack
addStackHint :: Type Stack -> Hint -> Type Stack
addStackHint type_ hint = case type_ of
  r :. t -> addStackHint r hint :. (t `addHint` hint)
  _ -> type_

-- | Gets the bottommost element of a stack type.
bottommost :: Type Stack -> Type Stack
bottommost type_ = case type_ of
  (a :. _) -> bottommost a
  _ -> type_

mono :: a -> Scheme a
mono = Forall S.empty S.empty

stack :: Id TypeSpace -> TypeId Stack
stack = TypeId

scalar :: Id TypeSpace -> TypeId Scalar
scalar = TypeId

unScheme :: Scheme a -> a
unScheme (Forall _ _ x) = x
