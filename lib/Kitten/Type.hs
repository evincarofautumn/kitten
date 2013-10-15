{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Kitten.Type
  ( module Kitten.Kind
  , Annotated(..)
  , Hint(..)
  , Origin(..)
  , Scheme(..)
  , Type(..)
  , TypeName(..)
  , TypeScheme
  , (-->)
  , addHint
  , mono
  , row
  , scalar
  , unScheme
  ) where

import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

import Kitten.Builtin (Builtin)
import Kitten.Kind
import Kitten.Location
import Kitten.Name
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Util.Text as T

data Type (a :: Kind) where
  (:&) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  (:.) :: !(Type Row) -> !(Type Scalar) -> Type Row
  (:?) :: !(Type Scalar) -> Type Scalar
  (:|) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  Bool :: !Origin -> Type Scalar
  Char :: !Origin -> Type Scalar
  Const :: !(TypeName a) -> !Origin -> Type a
  Empty :: !Origin -> Type Row
  Float :: !Origin -> Type Scalar
  Function :: !(Type Row) -> !(Type Row) -> !Origin -> Type Scalar
  Handle :: !Origin -> Type Scalar
  Int :: !Origin -> Type Scalar
  Named :: !Text -> !Origin -> Type Scalar
  Quantified :: !TypeScheme -> !Origin -> Type Scalar
  Unit :: !Origin -> Type Scalar
  Var :: !(TypeName a) -> !Origin -> Type a
  Vector :: !(Type Scalar) -> !Origin -> Type Scalar

instance Eq (Type a) where
  (a :& b) == (c :& d) = (a, b) == (c, d)
  (a :. b) == (c :. d) = (a, b) == (c, d)
  (:?) a == (:?) b = a == b
  (a :| b) == (c :| d) = (a, b) == (c, d)
  Bool{} == Bool{} = True
  Char{} == Char{} = True
  Const a _ == Const b _ = a == b
  Empty{} == Empty{} = True
  Float{} == Float{} = True
  Function a b _ == Function c d _ = (a, b) == (c, d)
  Handle{} == Handle{} = True
  Int{} == Int{} = True
  Named a _ == Named b _ = a == b
  Unit{} == Unit{} = True
  Var a _ == Var b _ = a == b
  Vector a _ == Vector b _ = a == b
  _ == _ = False

instance Show (Type Scalar) where
  show = T.unpack . toText

instance Show (Type Row) where
  show = T.unpack . toText

-- TODO showsPrec
instance ToText (Type Scalar) where
  toText = \case
    t1 :& t2 -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    (:?) t -> toText t <> "?"
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    Bool o -> "Bool" <> suffix o
    Char o -> "Char" <> suffix o
    Const (TypeName (Name index)) o
      -> "t" <> showText index <> suffix o  -- TODO Show differently?
    Float o -> "Float" <> suffix o
    Function r1 r2 o -> T.concat
      [ "(", T.unwords [toText r1, "->", toText r2], ")"
      , suffix o
      ]
    Handle o -> "Handle" <> suffix o
    Int o -> "Int" <> suffix o
    Named name o -> name <> suffix o
    Quantified scheme _ -> toText scheme
    Unit o -> "()" <> suffix o
    Var (TypeName (Name index)) o
      -> "t" <> showText index <> suffix o
    Vector t o -> T.concat ["[", toText t, "]", suffix o]

instance ToText (Type Row) where
  toText = \case
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    Const (TypeName (Name index)) o
      -> ".r" <> showText index <> suffix o
    Empty o -> "<empty>" <> suffix o
    Var (TypeName (Name index)) o
      -> ".r" <> showText index <> suffix o

suffix :: Origin -> Text
suffix (Origin hint _) = case hint of
  Local name -> " (type of " <> name <> ")"
  AnnoType annotated
    -> " (type of " <> toText annotated <> ")"
  AnnoVar name annotated
    -> " (type var " <> name <> " of " <> toText annotated <> ")"
  AnnoFunctionInput annotated
    -> " (input to " <> toText annotated <> ")"
  AnnoFunctionOutput annotated
    -> " (output of " <> toText annotated <> ")"
  NoHint -> ""

newtype TypeName (a :: Kind) = TypeName { unTypeName :: Name }
  deriving (Eq, Ord)

instance Show (TypeName a) where
  show = T.unpack . toText

instance ToText (TypeName a) where
  toText = toText . unTypeName

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
  toText (Builtin builtin) = showText builtin

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

data Scheme a = Forall
  (Set (TypeName Row))
  (Set (TypeName Scalar))
  a
  deriving (Eq, Functor)

instance (ToText a) => Show (Scheme a) where
  show = T.unpack . toText

instance (ToText a) => ToText (Scheme a) where
  toText (Forall rows scalars type_) = T.unwords
    [ "forall"
    , wordSetText rows
    , wordSetText scalars
    , "."
    , toText type_
    ]
    where
    wordSetText :: Set (TypeName a) -> Text
    wordSetText = T.unwords . map toText . S.toList

type TypeScheme = Scheme (Type Scalar)

infix 6 :&
infix 6 :|
infixl 5 :.
infix 4 -->

-- | Creates a 'Function' scalar type, inferring hints from
-- the given 'Origin'.
hintedFunction :: Type Row -> Type Row -> Origin -> Type Scalar
hintedFunction inputs outputs origin
  = Function inputs' outputs' origin
  where
  inputs', outputs' :: Type Row
  (inputs', outputs') = case origin of
    Origin (AnnoType anno) _
      ->
        ( inputs `addRowHint` AnnoFunctionInput anno
        , outputs `addRowHint` AnnoFunctionOutput anno
        )
    _ -> (inputs, outputs)

(-->) :: Type Row -> Type Row -> Origin -> Type Scalar
(a --> b) origin = hintedFunction a b origin

addHint :: Type a -> Hint -> Type a
addHint type_ hint = case type_ of
  _ :& _ -> type_
  _ :. _ -> type_
  (:?) _ -> type_
  _ :| _ -> type_
  Empty o -> Empty (f o)
  Bool o -> Bool (f o)
  Char o -> Char (f o)
  Const name o -> Const name (f o)
  Float o -> Float (f o)
  Function r1 r2 o -> Function r1 r2 (f o)
  Handle o -> Handle (f o)
  Int o -> Int (f o)
  Named name o -> Named name (f o)
  Quantified scheme o -> Quantified scheme o
  Unit o -> Unit (f o)
  Var name o -> Var name (f o)
  Vector t o -> Vector t (f o)
  where
  f :: Origin -> Origin
  f (Origin _hint loc) = Origin hint loc

-- | Adds a 'Hint' to each 'Type Scalar' along a 'Type Row'.
-- Shallow in scalars, deep in rows.
--
-- TODO(strager): mapRow
addRowHint :: Type Row -> Hint -> Type Row
addRowHint type_ hint = case type_ of
  r :. t -> addRowHint r hint :. (t `addHint` hint)
  _ -> type_

mono :: a -> Scheme a
mono = Forall S.empty S.empty

row :: Name -> TypeName Row
row = TypeName

scalar :: Name -> TypeName Scalar
scalar = TypeName

unScheme :: Scheme a -> a
unScheme (Forall _ _ x) = x
