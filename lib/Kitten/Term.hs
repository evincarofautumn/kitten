{-|
Module      : Kitten.Term
Description : The core language
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kitten.Term
  ( Annotation
  , Case(..)
  , CoercionHint(..)
  , Else(..)
  , MatchHint(..)
  , Permit(..)
  , Sweet(..)
  , Term(..)
  , Value(..)
  , asCoercion
  , compose
  , composed
  , decompose
  , decomposed
  , fromInfix
  , identityCoercion
  , permissionCoercion
  , quantifierCount
  , quantifierCount'
  , stripMetadata
  , stripValue
  , type_
  ) where

import Data.List (intersperse, partition)
import Data.Text (Text)
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Literal (IntegerLiteral, FloatLiteral)
import Kitten.Name
import Kitten.Operator (Fixity)
import Kitten.Origin (HasOrigin(..), Origin)
import Kitten.Phase (Phase(..))
import Kitten.Signature (Signature)
import Kitten.Type (Type, TypeId)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as Text
import qualified Kitten.Kind as Kind
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Signature as Signature
import qualified Text.PrettyPrint as Pretty

type family Annotation (p :: Phase) :: * where
  Annotation 'Parsed = ()
  Annotation 'Resolved = ()
  Annotation 'Postfix = ()
  Annotation 'Scoped = ()
  Annotation 'Typed = Type

type family HasInfix (p :: Phase) :: Bool where
  HasInfix 'Parsed = 'False
  HasInfix 'Resolved = 'False
  HasInfix 'Postfix = 'True
  HasInfix 'Scoped = 'True
  HasInfix 'Typed = 'True

data Sweet (p :: Phase)

  = SArray          -- Unboxed array literal
    (Annotation p)
    !Origin
    [Sweet p]       -- Elements

  | SAs             -- Type annotation
    (Annotation p)
    !Origin
    [Signature]     -- Types

  | SCharacter      -- ASCII text literal (straight quotes)
    (Annotation p)
    !Origin
    !Text           -- Escaped character (e.g., "\t" for literal tab vs. "\\t" for escape)

  | SCompose
    (Annotation p)
    !(Sweet p)
    !(Sweet p)

  | SDo             -- Prefix function call
    (Annotation p)
    !Origin
    !(Sweet p)      -- Function
    !(Sweet p)      -- Argument

  | SEscape         -- Single-element quotation (\name, \(@ foo))
    (Annotation p)
    !Origin
    !(Sweet p)

  | SFloat
    (Annotation p)
    !Origin
    !FloatLiteral

  | {- (HasTypes p ~ 'True) => -} SGeneric  -- Local generic type variables
    (Annotation p)
    !Origin
    !Unqualified                            -- Type name
    !TypeId                                 -- Type ID (could be removed?)
    !(Sweet p)                              -- Body

  | SGroup
    (Annotation p)
    !Origin
    !(Sweet p)

  | SIdentity
    (Annotation p)
    !Origin

  | SIf
    (Annotation p)
    !Origin
    !(Maybe (Sweet p))    -- Condition
    !(Sweet p)            -- True branch
    [(Origin, Sweet p, Sweet p)]  -- elif branches
    !(Maybe (Sweet p))    -- else branch

  | (HasInfix p ~ 'True) => SInfix  -- Desugared infix operator (only present after infix desugaring)
    (Annotation p)
    !Origin
    !(Sweet p)                      -- Left operand
    !GeneralName                    -- Operator
    !(Sweet p)                      -- Right operand

  | SInteger
    (Annotation p)
    !Origin
    !IntegerLiteral

  | SJump
    (Annotation p)
    !Origin

  | SLambda                                      -- Local variable
    (Annotation p)
    !Origin
    [(Origin, Maybe Unqualified, Annotation p)]  -- Names or ignores (TODO: allow signatures)
    !(Sweet p)                                   -- Body

  | SList           -- Boxed list literal
    (Annotation p)
    !Origin
    [Sweet p]       -- List elements

  | SLocal          -- Push local
    (Annotation p)
    !Origin
    !Unqualified    -- Local name

  | SLoop
    (Annotation p)
    !Origin

  | SMatch
    (Annotation p)
    !Origin
    !(Maybe (Sweet p))                -- Scrutinee
    [(Origin, GeneralName, Sweet p)]  -- case branches
    !(Maybe (Sweet p))                -- else branch

  | SNestableCharacter  -- Nestable character literal (round quotes)
    (Annotation p)
    !Origin
    !Text               -- Escaped character (e.g., " " vs. ""

  | SNestableText   -- Nestable text literal (round quotes)
    (Annotation p)
    !Origin
    !Text           -- Escaped text

  | SParagraph      -- Paragraph literal
    (Annotation p)
    !Origin
    !Text           -- Escaped text ("\n" for line breaks, "\\n" for escapes)

  | STag               -- Tag fields to make new ADT instance
    (Annotation p)
    !Origin
    !Int               -- Number of fields
    !ConstructorIndex  -- Constructor tag

  | SText           -- Text literal (straight quotes)
    (Annotation p)
    !Origin
    !Text           -- Escaped text

  | SQuotation      -- Boxed quotation
    (Annotation p)
    !Origin
    !(Sweet p)

  | SReturn
    (Annotation p)
    !Origin

  | SSection                       -- Operator section
    (Annotation p)
    !Origin
    !GeneralName                   -- Operator
    !(Either (Sweet p) (Sweet p))  -- Operand

  | STodo           -- ... expression
    (Annotation p)
    !Origin

  | SUnboxedQuotation
    (Annotation p)
    !Origin
    !(Sweet p)

  | SWith           -- Permission coercion
    (Annotation p)
    !Origin
    [Permit]        -- Permissions to add or remove

  | SWord
    (Annotation p)
    !Origin
    !Fixity         -- Whether used infix or postfix at call site
    !GeneralName
    [Signature]     -- Type arguments

deriving instance (Eq (Annotation p)) => Eq (Sweet p)
deriving instance (Show (Annotation p)) => Show (Sweet p)

fromInfix :: Sweet p -> Maybe (Sweet 'Postfix)
fromInfix SInfix{} = Nothing
fromInfix x = Just (unsafeCoerce x)

instance HasOrigin (Sweet p) where
  getOrigin term = case term of
    SArray _ o _ -> o
    SAs _ o _ -> o
    SCharacter _ o _ -> o
    SCompose _ a _ -> getOrigin a
    SDo _ o _ _ -> o
    SEscape _ o _ -> o
    SFloat _ o _ -> o
    SGeneric _ o _ _ _ -> o
    SGroup _ o _ -> o
    SIdentity _ o -> o
    SIf _ o _ _ _ _ -> o
    SInfix _ o _ _ _ -> o
    SInteger _ o _ -> o
    SJump _ o -> o
    SLambda _ o _ _ -> o
    SList _ o _ -> o
    SLocal _ o _ -> o
    SLoop _ o -> o
    SMatch _ o _ _ _ -> o
    SNestableCharacter _ o _ -> o
    SNestableText _ o _ -> o
    SParagraph _ o _ -> o
    STag _ o _ _ -> o
    SText _ o _ -> o
    SQuotation _ o _ -> o
    SReturn _ o -> o
    SSection _ o _ _ -> o
    STodo _ o -> o
    SUnboxedQuotation _ o _ -> o
    SWith _ o _ -> o
    SWord _ o _ _ _ -> o

instance Pretty (Sweet p) where
  pPrint term = case term of
    SArray _ _ items -> Pretty.hcat
      ["[|", Pretty.list $ map pPrint items, "|]"]

    SAs _ _ types -> Pretty.hcat
      ["as (", Pretty.list $ map pPrint types, ")"]

    SCharacter _ _ c -> Pretty.quotes $ Pretty.text $ Text.unpack c

    SCompose _ a b -> Pretty.sep $ map pPrint [a, b]

    SDo _ _ f x -> Pretty.vcat
      [ Pretty.hcat ["do (", pPrint f, ")"]
      -- See note [Block Pretty-printing].
      , Pretty.nest 4 $ pPrint x
      ]

    SEscape _ _ x -> Pretty.hcat ["\\", pPrint x]

    SFloat _ _ literal -> pPrint literal

    -- TODO: Should we pretty-print the generic quantifiers?
    SGeneric _ _ _ _ x -> pPrint x

    SGroup _ _ x -> Pretty.parens $ pPrint x

    SIdentity{} -> ""

    SIf _ _ mCondition true elifs mElse -> Pretty.vcat
      [ "if"
      , case mCondition of
        Just condition -> Pretty.hcat [" (", pPrint condition, ")"]
        Nothing -> ""
      -- See note [Block Pretty-printing].
      , Pretty.nest 4 $ pPrint true
      , Pretty.vcat $ map elif elifs
      , case mElse of
        -- See note [Block Pretty-printing].
        Just else_ -> Pretty.vcat ["else", Pretty.nest 4 $ pPrint else_]
        Nothing -> ""
      ]
      where
        elif (_, condition, body) = Pretty.vcat
          [ Pretty.hcat ["elif (", pPrint condition, ")"]
          -- See note [Block Pretty-printing].
          , Pretty.nest 4 $ pPrint body
          ]

    SInfix _ _ a op b -> Pretty.parens
      $ Pretty.sep [pPrint a, pPrint op, pPrint b]

    SInteger _ _ literal -> pPrint literal

    SJump{} -> "jump"

    SLambda _ _ vars body -> Pretty.vcat
      [ Pretty.hcat ["-> ", Pretty.list $ map var vars, ";"]
      , pPrint body
      ]
      where
        var (_, mName, _) = case mName of
          Just name -> pPrint name
          Nothing -> "_"

    SList _ _ items -> Pretty.hcat
      ["[", Pretty.list $ map pPrint items, "]"]

    SLocal _ _ name -> pPrint name

    SLoop{} -> "loop"

    SMatch _ _ mScrutinee cases mElse -> Pretty.vcat
      [ "match"
      , case mScrutinee of
        Just scrutinee -> Pretty.hcat [" (", pPrint scrutinee, ")"]
        Nothing -> ""
      , Pretty.vcat $ map case_ cases
      , case mElse of
        -- See note [Block Pretty-printing].
        Just else_ -> Pretty.vcat ["else", Pretty.nest 4 $ pPrint else_]
        Nothing -> ""
      ]
      where
        case_ (_, name, body) = Pretty.vcat
          [ Pretty.hcat ["case ", pPrint name]
          -- See note [Block Pretty-printing].
          , Pretty.nest 4 $ pPrint body
          ]

    SNestableCharacter _ _ c -> Pretty.hcat
      ["\x2018", Pretty.text $ Text.unpack c, "\x2019"]

    SNestableText _ _ t -> Pretty.hcat
      ["\x201C", Pretty.text $ Text.unpack t, "\x201D"]

    SParagraph _ _ t -> Pretty.vcat
      $ "\"\"\""
      : map (Pretty.text . Text.unpack) (Text.lines t)
      ++ ["\"\"\""]

    -- It's fine that this isn't valid syntax, because a user should never need
    -- to write it.
    STag _ _ _ (ConstructorIndex index) -> Pretty.hcat ["#", pPrint index]

    SText _ _ t -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t

    -- See note [Block Pretty-printing].
    SQuotation _ _ x -> Pretty.hsep ["{", pPrint x, "}"]

    SReturn{} -> "return"

    SSection _ _ name operand -> Pretty.parens
      $ case operand of
        Left x -> Pretty.hsep [pPrint x, pPrint name]
        Right x -> Pretty.sep [pPrint name, pPrint x]

    STodo{} -> "..."

    -- See note [Block Pretty-printing].
    SUnboxedQuotation _ _ x -> Pretty.hsep ["{|", pPrint x, "|}"]

    SWith _ _ permits -> Pretty.hcat
      ["with (", Pretty.list $ map permit permits, ")"]
      where
        permit p = Pretty.hcat
          [ if permitted p then "+" else "-"
          , pPrint $ permitName p
          ]

    -- TODO: Incorporate fixity and type arguments.
    SWord _ _ _fixity name _typeArgs -> pPrint name

-- | This is the core language. It permits pushing values to the stack, invoking
-- definitions, and moving values between the stack and local variables.
--
-- It also permits empty programs and program concatenation. Together these form
-- a monoid over programs. The denotation of the concatenation of two programs
-- is the composition of the denotations of those two programs. In other words,
-- there is a homomorphism from the syntactic monoid onto the semantic monoid.
--
-- A value of type @'Term' a@ is a term annotated with a value of type @a@. A
-- parsed term may have a type like @'Term' ()@, while a type-inferred term may
-- have a type like @'Term' 'Type'@.

data Term a
  -- | @id@, @as (T)@, @with (+A -B)@: coerces the stack to a particular type.
  = Coercion !CoercionHint a !Origin
  -- | @e1 e2@: composes two terms.
  | Compose a !(Term a) !(Term a)
  -- | @Λx. e@: generic terms that can be specialized.
  | Generic !Unqualified !TypeId !(Term a) !Origin
  -- | @(e)@: precedence grouping for infix operators.
  | Group !(Term a)
  -- | @→ x; e@: local variable introductions.
  | Lambda a !Unqualified a !(Term a) !Origin
  -- | @match { case C {...}... else {...} }@, @if {...} else {...}@:
  -- pattern-matching.
  | Match !MatchHint a [Case a] !(Else a) !Origin
  -- | @new.n@: ADT allocation.
  | New a !ConstructorIndex !Int !Origin
  -- | @new.closure.n@: closure allocation.
  | NewClosure a !Int !Origin
  -- | @new.vec.n@: vector allocation.
  | NewVector a !Int a !Origin
  -- | @push v@: push of a value.
  | Push a !(Value a) !Origin
  -- | @f@: an invocation of a word.
  | Word a !Fixity !GeneralName [Type] !Origin
  deriving (Eq, Show)

-- | The type of coercion to perform.

data CoercionHint
  -- | The identity coercion, generated by empty terms.
  = IdentityCoercion
  -- | A coercion to a particular type.
  | AnyCoercion !Signature
  deriving (Eq, Show)

-- | The original source of a @match@ expression

data MatchHint
  -- | @match@ generated from @if@.
  = BooleanMatch
  -- | @match@ explicitly in the source.
  | AnyMatch
  deriving (Eq, Show)

-- | A case branch in a @match@ expression.

data Case a = Case !GeneralName !(Term a) !Origin
  deriving (Eq, Show)

-- | An @else@ branch in a @match@ (or @if@) expression.

data Else a = Else !(Term a) !Origin
  deriving (Eq, Show)

-- | A permission to grant or revoke in a @with@ expression.

data Permit = Permit
  { permitted :: !Bool
  , permitName :: !GeneralName
  } deriving (Eq, Show)

-- | A value, used to represent literals in a parsed program, as well as runtime
-- values in the interpreter.

data Value a
  -- | A quotation with explicit variable capture; see "Kitten.Scope".
  = Capture [Closed] !(Term a)
  -- | A character literal.
  | Character !Char
  -- | A captured variable.
  | Closed !ClosureIndex
  -- | A floating-point literal.
  | Float !FloatLiteral
  -- | An integer literal.
  | Integer !IntegerLiteral
  -- | A local variable.
  | Local !LocalIndex
  -- | A reference to a name.
  | Name !Qualified
  -- | A parsed quotation.
  | Quotation !(Term a)
  -- | A text literal.
  | Text !Text
  deriving (Eq, Show)

-- FIXME: 'compose' should work on 'Term ()'.
compose :: a -> Origin -> [Term a] -> Term a
compose x o = foldr (Compose x) (identityCoercion x o)

composed :: (Annotation p ~ ()) => Origin -> [Sweet p] -> Sweet p
composed o = foldr (SCompose ()) (SIdentity () o)

asCoercion :: a -> Origin -> [Signature] -> Term a
asCoercion x o ts = Coercion (AnyCoercion signature) x o
  where
  signature = Signature.Quantified [] (Signature.Function ts ts [] o) o

identityCoercion :: a -> Origin -> Term a
identityCoercion = Coercion IdentityCoercion

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
decompose (Generic _name _id t _origin) = decompose t
decompose (Compose _ a b) = decompose a ++ decompose b
decompose (Coercion IdentityCoercion _ _) = []
decompose term = [term]

decomposed :: Sweet p -> [Sweet p]
-- FIXME: This shouldn't be necessary.
decomposed (SGeneric _ _origin _name _id body) = decomposed body
decomposed (SCompose _ a b) = decomposed a ++ decomposed b
decomposed SIdentity{} = []
decomposed term = [term]

instance HasOrigin (Term a) where
  getOrigin term = case term of
    Coercion _ _ o -> o
    Compose _ a _ -> getOrigin a
    Generic _ _ _ o -> o
    Group a -> getOrigin a
    Lambda _ _ _ _ o -> o
    New _ _ _ o -> o
    NewClosure _ _ o -> o
    NewVector _ _ _ o -> o
    Match _ _ _ _ o -> o
    Push _ _ o -> o
    Word _ _ _ _ o -> o

quantifierCount :: Term a -> Int
quantifierCount = countFrom 0
  where
  countFrom !count (Generic _ _ body _) = countFrom (count + 1) body
  countFrom count _ = count

quantifierCount' :: Sweet 'Typed -> Int
quantifierCount' = countFrom 0
  where
  countFrom !count (SGeneric _ _ _ _ body) = countFrom (count + 1) body
  countFrom count _ = count

-- Deduces the explicit type of a term.

type_ :: Term Type -> Type
type_ = metadata

metadata :: Term a -> a
metadata term = case term of
  Coercion _ t _ -> t
  Compose t _ _ -> t
  Generic _ _ term' _ -> metadata term'
  Group term' -> metadata term'
  Lambda t _ _ _ _ -> t
  Match _ t _ _ _ -> t
  New t _ _ _ -> t
  NewClosure t _ _ -> t
  NewVector t _ _ _ -> t
  Push t _ _ -> t
  Word t _ _ _ _ -> t

stripMetadata :: Term a -> Term ()
stripMetadata term = case term of
  Coercion a _ b -> Coercion a () b
  Compose _ a b -> Compose () (stripMetadata a) (stripMetadata b)
  Generic a b term' c -> Generic a b (stripMetadata term') c
  Group term' -> stripMetadata term'
  Lambda _ a _ b c -> Lambda () a () (stripMetadata b) c
  Match a _ b c d -> Match a () (map stripCase b) (stripElse c) d
  New _ a b c -> New () a b c
  NewClosure _ a b -> NewClosure () a b
  NewVector _ a _ b -> NewVector () a () b
  Push _ a b -> Push () (stripValue a) b
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
  Capture a b -> Capture a (stripMetadata b)
  Character a -> Character a
  Closed a -> Closed a
  Float a -> Float a
  Integer a -> Integer a
  Local a -> Local a
  Name a -> Name a
  Quotation a -> Quotation (stripMetadata a)
  Text a -> Text a

instance Pretty (Term a) where
  pPrint term = case term of
    Coercion{} -> Pretty.empty
    Compose _ a b -> pPrint a Pretty.$+$ pPrint b
    Generic name i body _ -> Pretty.hsep
      [ Pretty.angles $ Pretty.hcat [pPrint name, "/*", pPrint i, "*/"]
      , pPrint body
      ]
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
    Capture names term -> Pretty.hcat
      [ Pretty.char '$'
      , Pretty.parens $ Pretty.list $ map pPrint names
      , Pretty.braces $ pPrint term
      ]
    Character c -> Pretty.quotes $ Pretty.char c
    Closed (ClosureIndex index) -> "closure." Pretty.<> Pretty.int index
    Float f -> pPrint f
    Integer i -> pPrint i
    Local (LocalIndex index) -> "local." Pretty.<> Pretty.int index
    Name n -> Pretty.hcat ["\\", pPrint n]
    Quotation body -> Pretty.braces $ pPrint body
    Text t -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t
