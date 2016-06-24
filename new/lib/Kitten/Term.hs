{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Term
  ( Case(..)
  , CoercionHint(..)
  , Else(..)
  , MatchHint(..)
  , Permit(..)
  , Term(..)
  , Value(..)
  , compose
  , decompose
  , identityCoercion
  , origin
  , permissionCoercion
  , quantifierCount
  , stripMetadata
  , stripValue
  , type_
  ) where

import Data.List (intersperse, partition)
import Data.Text (Text)
import Data.Vector (Vector)
import Kitten.Bits
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Name
import Kitten.Operator (Fixity)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Type (Type, TypeId)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Kitten.Kind as Kind
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Signature as Signature
import qualified Text.PrettyPrint as Pretty

-- This is the core language. It permits pushing values to the stack, invoking
-- definitions, and moving values between the stack and local variables.
--
-- It also permits empty programs and program concatenation. Together these form
-- a monoid over programs. The denotation of the concatenation of two programs
-- is the composition of the denotations of those two programs. In other words,
-- there is a homomorphism from the syntactic monoid onto the semantic monoid.
--
-- A value of type 'Term a' is a term annotated with a value of type 'a'. A
-- parsed term may have a type like 'Term ()', while a type-inferred term may
-- have a type like 'Term Type'.

data Term a
  -- call
  = Call a !Origin
  -- id, as (T), with (+A -B)
  | Coercion !CoercionHint a !Origin
  -- e1 e2
  | Compose a !(Term a) !(Term a)
  -- drop
  | Drop a !Origin
  -- Λx. e
  | Generic !TypeId !(Term a) !Origin
  -- (e)
  | Group !(Term a)
  -- → x; e
  | Lambda a !Unqualified a !(Term a) !Origin
  -- match { case C {...}... else {...} }
  -- if {...} else {...}
  | Match !MatchHint a [Case a] !(Else a) !Origin
  -- new.n
  | New a !ConstructorIndex !Int !Origin
  -- new.closure.n
  | NewClosure a !Int !Origin
  -- new.vec.n
  | NewVector a !Int a !Origin
  -- push v
  | Push a !(Value a) !Origin
  -- swap
  | Swap a !Origin
  -- f
  | Word a !Fixity !GeneralName [Type] !Origin
  deriving (Eq, Show)

data CoercionHint
  = IdentityCoercion
  | AnyCoercion !Signature
  deriving (Eq, Show)

data MatchHint
  = BooleanMatch
  | AnyMatch
  deriving (Eq, Show)

data Case a = Case !GeneralName !(Term a) !Origin
  deriving (Eq, Show)

data Else a = Else !(Term a) !Origin
  deriving (Eq, Show)

data Permit = Permit
  { permitted :: !Bool
  , permitName :: !GeneralName
  } deriving (Eq, Show)

data Value a
  = Algebraic !ConstructorIndex [Value a]
  | Array !(Vector (Value a))
  | Capture [Closed] !(Term a)
  | Character !Char
  | Closed !ClosureIndex
  | Closure !Qualified [Value a]
  | Float !Double !FloatBits
  | Integer !Integer !IntegerBits
  | Local !LocalIndex
  | Name !Qualified
  | Quotation !(Term a)
  | Text !Text
  deriving (Eq, Show)

-- FIXME: 'compose' should work on 'Term ()'.
compose :: a -> Origin -> [Term a] -> Term a
compose x o = foldr (Compose x) (identityCoercion x o)

identityCoercion :: a -> Origin -> Term a
identityCoercion x o = Coercion IdentityCoercion x o

permissionCoercion :: [Permit] -> a -> Origin -> Term a
permissionCoercion permits x o = Coercion (AnyCoercion signature) x o
  where
  signature = Signature.Quantified
    [ Parameter o "R" Kind.Stack
    , Parameter o "S" Kind.Stack
    ]
    (Signature.Function
      [ Signature.StackFunction
        (Signature.Variable "R" o) []
        (Signature.Variable "S" o) []
        (map permitName grants) o
      ]
      [ Signature.StackFunction
        (Signature.Variable "R" o) []
        (Signature.Variable "S" o) []
        (map permitName revokes) o
      ]
      [] o) o
  (grants, revokes) = partition permitted permits

decompose :: Term a -> [Term a]
-- TODO: Verify that this is correct.
decompose (Generic _ t _) = decompose t
decompose (Compose _ a b) = decompose a ++ decompose b
decompose (Coercion IdentityCoercion _ _) = []
decompose term = [term]

origin :: Term a -> Origin
origin term = case term of
  Call _ o -> o
  Coercion _ _ o -> o
  Compose _ a _ -> origin a
  Drop _ o -> o
  Generic _ _ o -> o
  Group a -> origin a
  Lambda _ _ _ _ o -> o
  New _ _ _ o -> o
  NewClosure _ _ o -> o
  NewVector _ _ _ o -> o
  Match _ _ _ _ o -> o
  Push _ _ o -> o
  Swap _ o -> o
  Word _ _ _ _ o -> o

quantifierCount :: Term a -> Int
quantifierCount = countFrom 0
  where
  countFrom !count (Generic _ body _) = countFrom (count + 1) body
  countFrom count _ = count

-- Deduces the explicit type of a term.

type_ :: Term Type -> Type
type_ = metadata

metadata :: Term a -> a
metadata term = case term of
  Call t _ -> t
  Coercion _ t _ -> t
  Compose t _ _ -> t
  Drop t _ -> t
  Generic _ term' _ -> metadata term'
  Group term' -> metadata term'
  Lambda t _ _ _ _ -> t
  Match _ t _ _ _ -> t
  New t _ _ _ -> t
  NewClosure t _ _ -> t
  NewVector t _ _ _ -> t
  Push t _ _ -> t
  Swap t _ -> t
  Word t _ _ _ _ -> t

stripMetadata :: Term a -> Term ()
stripMetadata term = case term of
  Call _ a -> Call () a
  Coercion a _ b -> Coercion a () b
  Compose _ a b -> Compose () (stripMetadata a) (stripMetadata b)
  Drop _ a -> Drop () a
  Generic a term' b -> Generic a (stripMetadata term') b
  Group term' -> stripMetadata term'
  Lambda _ a _ b c -> Lambda () a () (stripMetadata b) c
  Match a _ b c d -> Match a () (map stripCase b) (stripElse c) d
  New _ a b c -> New () a b c
  NewClosure _ a b -> NewClosure () a b
  NewVector _ a _ b -> NewVector () a () b
  Push _ a b -> Push () (stripValue a) b
  Swap _ a -> Swap () a
  Word _ a b c d -> Word () a b c d
  where

  stripCase :: Case a -> Case ()
  stripCase case_ = case case_ of
    Case a b c -> Case a (stripMetadata b) c

  stripElse :: Else a -> Else ()
  stripElse else_ = case else_ of
    Else a b -> Else (stripMetadata a) b

stripValue :: Value a -> Value ()
stripValue v = case v of
  Algebraic a b -> Algebraic a (map stripValue b)
  Array a -> Array (fmap stripValue a)
  Capture a b -> Capture a (stripMetadata b)
  Character a -> Character a
  Closed a -> Closed a
  Closure a b -> Closure a (map stripValue b)
  Float a b -> Float a b
  Integer a b -> Integer a b
  Local a -> Local a
  Name a -> Name a
  Quotation a -> Quotation (stripMetadata a)
  Text a -> Text a

instance Pretty (Term a) where
  pPrint term = case term of
    Call{} -> "call"
    Coercion{} -> Pretty.empty
    Compose _ a b -> pPrint a Pretty.$+$ pPrint b
    Drop _ _ -> "drop"
    Generic name body _ -> Pretty.hsep
      [Pretty.angles $ pPrint name, pPrint body]
    Group a -> Pretty.parens (pPrint a)
    Lambda _ name _ body _ -> "->"
      Pretty.<+> pPrint name
      Pretty.<> ";"
      Pretty.$+$ pPrint body
    Match _ _ cases else_ _ -> Pretty.vcat
      [ "match:"
      , Pretty.nest 4 $ Pretty.vcat $ map pPrint cases
        ++ [pPrint else_]
      ]
    New _ (ConstructorIndex index) _size _ -> "new." Pretty.<> Pretty.int index
    NewClosure _ size _ -> "new.closure." Pretty.<> pPrint size
    NewVector _ size _ _ -> "new.vec." Pretty.<> pPrint size
    Push _ value _ -> pPrint value
    Swap{} -> "swap"
    Word _ _ name [] _ -> pPrint name
    Word _ _ name args _ -> Pretty.hcat
      $ pPrint name : "::<" : intersperse ", " (map pPrint args) ++ [">"]

instance Pretty (Case a) where
  pPrint (Case name body _) = Pretty.vcat
    [ Pretty.hcat ["case ", pPrint name, ":"]
    , Pretty.nest 4 $ pPrint body
    ]

instance Pretty (Else a) where
  pPrint (Else body _) = Pretty.vcat ["else:", Pretty.nest 4 $ pPrint body]

instance Pretty Permit where
  pPrint (Permit allow name) = Pretty.hcat
    [if allow then "+" else "-", pPrint name]

instance Pretty (Value a) where
  pPrint value = case value of
    Algebraic{} -> "<adt>"
    Array values -> Pretty.brackets $ Pretty.list
      $ Vector.toList $ fmap pPrint values
    Capture names term -> Pretty.hcat
      [ Pretty.char '$'
      , Pretty.parens $ Pretty.list $ map pPrint names
      , Pretty.braces $ pPrint term
      ]
    Character c -> Pretty.quotes $ Pretty.char c
    Closed (ClosureIndex index) -> "closure." Pretty.<> Pretty.int index
    Closure{} -> "<closure>"
    Float f bits -> Pretty.hcat [Pretty.double f, pPrint bits]
    Integer i bits -> Pretty.hcat [Pretty.integer i, pPrint bits]
    Local (LocalIndex index) -> "local." Pretty.<> Pretty.int index
    Name n -> Pretty.hcat ["\\", pPrint n]
    Quotation body -> Pretty.braces $ pPrint body
    Text t -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t
