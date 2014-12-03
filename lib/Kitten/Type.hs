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
{-# LANGUAGE PostfixOperators #-}

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
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Name

import Kitten.Util.Text (ToText(..), showText)

data Type (a :: Kind) where
  TyApply :: !(Type Scalar) -> !(Vector (Type Scalar)) -> !Location -> Type Scalar
  TyConst :: !(KindedId a) -> !Location -> Type a
  TyCtor :: !Ctor -> !Location -> Type Scalar
  TyEmpty :: !Location -> Type Stack
  TyFunction :: !(Type Stack) -> !(Type Stack) -> !Location -> Type Scalar
  TyOption :: !(Type Scalar) -> !Location -> Type Scalar
  TyProduct :: !(Type Scalar) -> !(Type Scalar) -> !Location -> Type Scalar
  TyQuantified :: !TypeScheme -> !Location -> Type Scalar
  TyStack :: !(Type Stack) -> !(Type Scalar) -> Type Stack
  TySum :: !(Type Scalar) -> !(Type Scalar) -> !Location -> Type Scalar
  TyVar :: !(KindedId a) -> !Location -> Type a
  TyVector :: !(Type Scalar) -> !Location -> Type Scalar

typeLocation :: Type a -> Location
typeLocation = \case
  TyApply _ _ loc -> loc
  TyConst _ loc -> loc
  TyCtor _ loc -> loc
  TyEmpty loc -> loc
  TyFunction _ _ loc -> loc
  TyOption _ loc -> loc
  TyProduct _ _ loc -> loc
  TyQuantified _ loc -> loc
  TyStack _ a -> typeLocation a
  TySum _ _ loc -> loc
  TyVar _ loc -> loc
  TyVector _ loc -> loc

setTypeLocation :: Location -> Type a -> Type a
setTypeLocation loc = \case
  TyProduct a b _ -> TyProduct (recur a) (recur b) loc
  TyStack a b -> TyStack (recur a) (recur b)
  TyOption a _ -> TyOption (recur a) loc
  TyApply a b _ -> TyApply (recur a) (V.map recur b) loc
  TySum a b _ -> TySum (recur a) (recur b) loc
  a@TyConst{} -> a
  TyCtor a _ -> TyCtor a loc
  TyEmpty _ -> TyEmpty loc
  TyFunction a b _ -> TyFunction (recur a) (recur b) loc
  TyQuantified (Forall stacks scalars a) _
    -> TyQuantified (Forall stacks scalars (recur a)) loc
  a@TyVar{} -> a
  TyVector a _ -> TyVector (recur a) loc
  where
  recur :: Type a -> Type a
  recur = setTypeLocation loc

data Ctor
  = CtorBool
  | CtorChar
  | CtorFloat
  | CtorHandle
  | CtorInt
  | CtorUser !Name
  deriving (Eq)

tyBool, tyChar, tyFloat, tyHandle, tyInt :: Location -> Type Scalar
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
  TyConst a _ == TyConst b _ = a == b
  TyCtor a _ == TyCtor b _ = a == b
  TyEmpty{} == TyEmpty{} = True
  TyFunction a b _ == TyFunction c d _ = (a, b) == (c, d)
  TyOption a _ == TyOption b _ = a == b
  TyProduct a b _ == TyProduct c d _ = (a, b) == (c, d)
  TyQuantified a _ == TyQuantified b _ = a == b
  TyStack a b == TyStack c d = (a, b) == (c, d)
  TySum a b _ == TySum c d _ = (a, b) == (c, d)
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
    TyApply t1 ts _ -> toText t1 <> case V.toList ts of
      [] -> ""
      [t] -> "@" <> toText t
      ts' -> T.concat ["@(", T.intercalate ", " $ map toText ts', ")"]
    TyConst (KindedId (Id i)) _
      -> "t" <> showText i  -- TODO Show differently?
    TyCtor name _ -> toText name
    TyFunction r1 r2 _ -> T.concat
      ["(", T.unwords [toText r1, "->", toText r2], ")"]
    TyOption t _ -> toText t <> "?"
    TyProduct t1 t2 _ -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    TyQuantified scheme _ -> toText scheme
    TySum t1 t2 _ -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    TyVar (KindedId (Id i)) _ -> "t" <> showText i
    TyVector t _ -> T.concat ["[", toText t, "]"]

instance ToText (Type Stack) where
  toText = \case
    TyConst (KindedId (Id i)) _
      -> "s" <> showText i <> "..."  -- TODO Show differently?
    TyEmpty _ -> "<empty>"
    TyStack t1 t2 -> T.unwords [toText t1, toText t2]
    TyVar (KindedId (Id i)) _ -> "s" <> showText i <> "..."

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
    wordSetText = map (toText . flip TyVar loc) . S.toList

    loc = Location
      { locationStart = newPos "" 0 0, locationIndent = -1 }

type TypeScheme = Scheme (Type Scalar)

infix 6 &:
infix 6 |:
infixl 5 -:
infix 4 -->

(-->) :: Type Stack -> Type Stack -> Location -> Type Scalar
(-->) = TyFunction

(&:), (|:) :: Type Scalar -> Type Scalar -> Location -> Type Scalar
(&:) = TyProduct
(|:) = TySum

(-:) :: Type Stack -> Type Scalar -> Type Stack
(-:) = TyStack

-- | Gets the bottommost element of a stack type.
bottommost :: Type Stack -> Type Stack
bottommost type_ = case type_ of
  TyStack a _ -> bottommost a
  _ -> type_

mono :: a -> Scheme a
mono = Forall S.empty S.empty

stackVar :: TypeId -> KindedId Stack
stackVar = KindedId

scalarVar :: TypeId -> KindedId Scalar
scalarVar = KindedId

unscheme :: Scheme a -> a
unscheme (Forall _ _ x) = x
