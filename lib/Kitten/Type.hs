{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type where

import Data.Foldable (Foldable)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (Traversable)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Id
import Kitten.Intrinsic
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Name

import Kitten.Util.Text (ToText(..), showText)

data Type (a :: Kind) where
  (:&) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  (:.) :: !(Type Stack) -> !(Type Scalar) -> Type Stack
  (:?) :: !(Type Scalar) -> Type Scalar
  (:@) :: !(Type Scalar) -> !(Vector (Type Scalar)) -> Type Scalar
  (:|) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  TyConst :: !(KindedId a) -> !Origin -> Type a
  TyCtor :: !Ctor -> !Origin -> Type Scalar
  TyEmpty :: !Origin -> Type Stack
  TyFunction :: !(Type Stack) -> !(Type Stack) -> !Origin -> Type Scalar
  TyQuantified :: !TypeScheme -> !Origin -> Type Scalar
  TyVar :: !(KindedId a) -> !Origin -> Type a
  TyVector :: !(Type Scalar) -> !Origin -> Type Scalar

typeOrigin :: Type a -> Maybe Origin
typeOrigin = \case
  (:&){} -> Nothing
  (:.){} -> Nothing
  (:?){} -> Nothing
  (:@){} -> Nothing
  (:|){} -> Nothing
  TyConst _ o -> Just o
  TyCtor _ o -> Just o
  TyEmpty o -> Just o
  TyFunction _ _ o -> Just o
  TyQuantified _ o -> Just o
  TyVar _ o -> Just o
  TyVector _ o -> Just o

data Ctor
  = CtorBool
  | CtorChar
  | CtorFloat
  | CtorHandle
  | CtorInt
  | CtorUser !Name
  deriving (Eq)

tyBool, tyChar, tyFloat, tyHandle, tyInt :: Origin -> Type Scalar
tyBool = TyCtor CtorBool
tyChar = TyCtor CtorChar
tyFloat = TyCtor CtorFloat
tyHandle = TyCtor CtorHandle
tyInt = TyCtor CtorInt

instance ToText Ctor where
  toText = \case
    CtorBool -> "bool"
    CtorChar -> "char"
    CtorFloat -> "float"
    CtorHandle -> "handle"
    CtorInt -> "int"
    CtorUser a -> toText a

instance Show Ctor where
  show = T.unpack . toText

instance Eq (Type a) where
  (a :& b) == (c :& d) = (a, b) == (c, d)
  (a :. b) == (c :. d) = (a, b) == (c, d)
  (:?) a == (:?) b = a == b
  (a :| b) == (c :| d) = (a, b) == (c, d)
  TyConst a _ == TyConst b _ = a == b
  TyCtor a _ == TyCtor b _ = a == b
  TyEmpty{} == TyEmpty{} = True
  TyFunction a b _ == TyFunction c d _ = (a, b) == (c, d)
  TyQuantified a _ == TyQuantified b _ = a == b
  TyVar a _ == TyVar b _ = a == b
  TyVector a _ == TyVector b _ = a == b
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
    t1 :@ ts -> toText t1 <> case V.toList ts of
      [] -> ""
      [t] -> "@" <> toText t
      ts' -> T.concat ["@(", T.intercalate ", " $ map toText ts', ")"]
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    TyConst (KindedId (Id i)) _
      -> "t" <> showText i  -- TODO Show differently?
    TyCtor name _ -> toText name
    TyFunction r1 r2 _ -> T.concat
      ["(", T.unwords [toText r1, "->", toText r2], ")"]
    TyQuantified scheme _ -> toText scheme
    TyVar (KindedId (Id i)) _ -> "t" <> showText i
    TyVector t _ -> T.concat ["[", toText t, "]"]

instance ToText (Type Stack) where
  toText = \case
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    TyConst (KindedId (Id i)) _
      -> "s" <> showText i <> "..."  -- TODO Show differently?
    TyEmpty _ -> "<empty>"
    TyVar (KindedId (Id i)) _ -> "s" <> showText i <> "..."

originSuffix :: Origin -> Text
originSuffix (Origin hint _) = case hint of
  HiLocal name -> " (type of " <> toText name <> ")"
  HiType annotated
    -> " (type of " <> toText annotated <> ")"
  HiVar _ annotated
    -> " (from " <> toText annotated <> ")"
  HiFunctionInput annotated
    -> " (input to " <> toText annotated <> ")"
  HiFunctionOutput annotated
    -> " (output of " <> toText annotated <> ")"
  HiNone -> ""

data Annotated
  = AnDef !Name
  | AnIntrinsic !Intrinsic

instance Show Annotated where
  show = T.unpack . toText

instance ToText Annotated where
  toText (AnDef defName) = toText defName
  toText (AnIntrinsic intrinsic) = toText intrinsic

data Origin = Origin
  { originHint :: !Hint
  , originLocation :: !Location
  } deriving (Show)

data Hint
  = HiLocal !Name
  -- ^ Name introduced by a 'Scoped' term.

  | HiType !Annotated
  -- ^ Explicit type annotation.

  | HiVar !Name !Annotated
  -- ^ Type variable in a type annotation.

  | HiFunctionInput !Annotated
  | HiFunctionOutput !Annotated

  | HiNone
  deriving (Show)

-- | Picks the most helpful hint.  'mappend' is commutative.
instance Monoid Hint where
  mempty = HiNone

  -- Note: patterns are ordered by preference.
  x@HiLocal{} `mappend` _ = x
  _ `mappend` x@HiLocal{} = x
  x@HiType{} `mappend` _ = x
  _ `mappend` x@HiType{} = x
  x@HiVar{} `mappend` _ = x
  _ `mappend` x@HiVar{} = x
  x@HiFunctionInput{} `mappend` _ = x
  _ `mappend` x@HiFunctionInput{} = x
  x@HiFunctionOutput{} `mappend` _ = x
  _ `mappend` x@HiFunctionOutput{} = x
  x@HiNone `mappend` _ = x

data StackHint
  = StackAny
  | Stack0
  | Stack1

-- FIXME Derived 'Eq' instance may be too restrictive.
data Scheme a = Forall
  (Set (KindedId Stack))
  (Set (KindedId Scalar))
  a
  deriving (Eq, Foldable, Functor, Traversable)

instance (ToText a) => Show (Scheme a) where
  show = T.unpack . toText

instance (ToText a) => ToText (Scheme a) where
  toText (Forall stacks scalars type_) = T.unwords
    $ (if null variables then [] else "@" : variables)
    ++ [toText type_]

    where
    variables :: [Text]
    variables = wordSetText stacks ++ wordSetText scalars

    wordSetText :: (ToText (Type a)) => Set (KindedId a) -> [Text]
    wordSetText = map (toText . flip TyVar origin) . S.toList

    origin = Origin
      { originHint = HiNone
      , originLocation = Location
        { locationStart = newPos "" 0 0, locationIndent = -1 }
      }

type TypeScheme = Scheme (Type Scalar)

infix 6 :&
infix 6 :|
infixl 5 :.
infix 4 -->

-- | Creates a 'Function' scalar type, inferring hints from
-- the given 'Origin'.
hintedFunction :: Type Stack -> Type Stack -> Origin -> Type Scalar
hintedFunction inputs outputs origin
  = TyFunction inputs' outputs' origin
  where
  inputs', outputs' :: Type Stack
  (inputs', outputs') = case origin of
    Origin (HiType anno) _ ->
      ( inputs `addStackHint` HiFunctionInput anno
      , outputs `addStackHint` HiFunctionOutput anno
      )
    _ -> (inputs, outputs)

(-->) :: Type Stack -> Type Stack -> Origin -> Type Scalar
(a --> b) origin = hintedFunction a b origin

addHint :: Type a -> Hint -> Type a
addHint type_ hint = case type_ of
  _ :& _ -> type_
  _ :. _ -> type_
  (:?) _ -> type_
  _ :@ _ -> type_
  _ :| _ -> type_
  TyEmpty o -> TyEmpty (f o)
  TyConst i o -> TyConst i (f o)
  TyCtor ctor o -> TyCtor ctor (f o)
  TyFunction r1 r2 o -> TyFunction r1 r2 (f o)
  TyQuantified scheme o -> TyQuantified scheme o
  TyVar i o -> TyVar i (f o)
  TyVector t o -> TyVector t (f o)
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

stackVar :: TypeId -> KindedId Stack
stackVar = KindedId

scalarVar :: TypeId -> KindedId Scalar
scalarVar = KindedId

unscheme :: Scheme a -> a
unscheme (Forall _ _ x) = x
