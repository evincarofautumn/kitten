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
  , Scheme(..)
  , Type(..)
  , TypeName(..)
  , TypeScheme
  , (-->)
  , (==>)
  , (+:)
  , effect
  , emptyScheme
  , mono
  , row
  , scalar
  , unScheme
  ) where

import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

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
  Bool :: !Location -> Type Scalar
  Char :: !Location -> Type Scalar
  Empty :: !Location -> Type Row
  Float :: !Location -> Type Scalar
  Function :: !(Type Row) -> !(Type Row) -> !(Type Effect) -> !Location -> Type Scalar
  Handle :: !Location -> Type Scalar
  Int :: !Location -> Type Scalar
  Named :: !Text -> !Location -> Type Scalar
  Unit :: !Location -> Type Scalar
  Var :: !(TypeName a) -> !Location -> Type a
  Vector :: !(Type Scalar) -> !Location -> Type Scalar

  (:+) :: !(Type Effect) -> !(Type Effect) -> Type Effect
  NoEffect :: !Location -> Type Effect
  IOEffect :: !Location -> Type Effect

instance Eq (Type a) where
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

instance Show (Type Scalar) where
  show = T.unpack . toText

instance Show (Type Row) where
  show = T.unpack . toText

instance Show (Type Effect) where
  show = T.unpack . toText

-- TODO showsPrec
instance ToText (Type Scalar) where
  toText = \case
    t1 :& t2 -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    (:?) t -> toText t <> "?"
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    Bool{} -> "Bool"
    Char{} -> "Char"
    Float{} -> "Float"
    Function r1 r2 e _ -> T.concat
      ["(", T.unwords [toText r1, "->", toText r2, "+", toText e], ")"]
    Handle{} -> "Handle"
    Int{} -> "Int"
    Named name _ -> name
    Var (TypeName (Name index)) _ -> "t" <> showText index
    Unit{} -> "()"
    Vector t _ -> T.concat ["[", toText t, "]"]

instance ToText (Type Row) where
  toText = \case
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    Empty{} -> "<empty>"
    Var (TypeName (Name index)) _ -> "r" <> showText index

instance ToText (Type Effect) where
  toText = \case
    t1 :+ t2 -> T.concat ["(", toText t1, " + ", toText t2, ")"]
    Var (TypeName (Name index)) _ -> "e" <> showText index
    NoEffect{} -> "()"
    IOEffect{} -> "IO"

newtype TypeName (a :: Kind) = TypeName { unTypeName :: Name }
  deriving (Eq, Ord)

instance Show (TypeName a) where
  show = T.unpack . toText

instance ToText (TypeName a) where
  toText = toText . unTypeName

data Scheme a = Forall
  (Set (TypeName Row))
  (Set (TypeName Scalar))
  (Set (TypeName Effect))
  a
  deriving (Eq, Functor)

instance (ToText a) => Show (Scheme a) where
  show = T.unpack . toText

instance (ToText a) => ToText (Scheme a) where
  toText (Forall rows scalars effects type_) = T.unwords
    [ "forall"
    , wordSetText rows
    , wordSetText scalars
    , wordSetText effects
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
infix 4 ==>

(-->) :: Type Row -> Type Row -> Location -> Type Scalar
(a --> b) loc = Function a b (NoEffect loc) loc

(==>) :: Type Row -> Type Row -> Location -> Type Scalar
(a ==> b) loc = Function a b (IOEffect loc) loc

(+:) :: Type Effect -> Type Effect -> Type Effect
a +: b | a == b = a
IOEffect loc +: _ = IOEffect loc
NoEffect _ +: a = a
_ +: IOEffect loc = IOEffect loc
a +: NoEffect _ = a
(a :+ b) +: c = a +: (b +: c)
a +: b = a :+ b

effect :: Name -> TypeName Effect
effect = TypeName

emptyScheme :: a -> Scheme a
emptyScheme = Forall S.empty S.empty S.empty

mono :: a -> Scheme a
mono = Forall S.empty S.empty S.empty

row :: Name -> TypeName Row
row = TypeName

scalar :: Name -> TypeName Scalar
scalar = TypeName

unScheme :: Scheme a -> a
unScheme (Forall _ _ _ x) = x
