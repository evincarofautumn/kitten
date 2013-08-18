{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

import Kitten.Kind
import Kitten.Location
import Kitten.Name
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Util.Text as T

data Type a where
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
  Test :: Type a
  Unit :: !Location -> Type Scalar
  Var :: !Name -> !Location -> Type a
  Vector :: !(Type Scalar) -> !Location -> Type Scalar

  (:+) :: !(Type Effect) -> !(Type Effect) -> Type Effect
  NoEffect :: !Location -> Type Effect
  IOEffect :: !Location -> Type Effect

newtype TypeName a = TypeName { typeName :: Name }
  deriving (Eq, Ord)

instance ToText (TypeName a) where
  toText = showText . typeName

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

data Scheme = Forall
  (Set (TypeName Row))
  (Set (TypeName Scalar))
  (Set (TypeName Effect))
  (Type Scalar)
  deriving (Eq)

instance ToText Scheme where
  toText (Forall rows scalars effects type_) = T.unwords
    [ "forall"
    , wordSetText rows
    , wordSetText scalars
    , wordSetText effects
    , toText type_
    ]
    where
    wordSetText :: Set (TypeName a) -> Text
    wordSetText = T.unwords . map toText . S.toList

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
  show = T.unpack . toText

instance ToText (Type a) where
  toText type_ = case type_ of
    t1 :& t2 -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    (:?) t -> toText t <> "?"
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    Empty{} -> "<empty>"
    Bool{} -> "Bool"
    Char{} -> "Char"
    Float{} -> "Float"
    Function r1 r2 e _ -> T.unwords [toText r1, "->", toText r2, "+", toText e]
    Handle{} -> "Handle"
    Int{} -> "Int"
    Named name _ -> name
    Test{} -> ""
    Var (Name index) _ -> "t" <> showText index
    Unit{} -> "()"
    Vector t _ -> T.concat ["[", toText t, "]"]

    t1 :+ t2 -> T.concat ["(", toText t1, " + ", toText t2, ")"]
    NoEffect{} -> "()"
    IOEffect{} -> "IO"

mono :: Type Scalar -> Scheme
mono = Forall S.empty S.empty S.empty
