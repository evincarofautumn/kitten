{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Tree
  ( Term(..)
  , Value(..)
  , ParsedTerm
  , ParsedValue
  , ResolvedTerm
  , ResolvedValue
  , TypedTerm
  , TypedValue
  , defTypeScheme
  , termMetadata
  , typedLocation
  , typedType
  ) where

import Control.Applicative ((<$))
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Location
import Kitten.Operator
import Kitten.Type (Kind(..), StackHint(..), Type, TypeScheme, unScheme)
import Kitten.Util.Text (ToText(..), showText)

-- TODO Add pretty 'Show' instance for 'Term's.
data Term a
  = Builtin !Builtin !a
  | Call !Fixity !Text !a
  | Compose !StackHint !(Vector (Term a)) !a
  | Lambda !Text !(Term a) !a
  | PairTerm !(Term a) !(Term a) !a
  | Push !(Value a) !a
  | VectorTerm !(Vector (Term a)) !a

instance (Eq a) => Eq (Term a) where
  Builtin a b == Builtin c d = (a, b) == (c, d)
  -- Calls are equal regardless of 'FixityHint'.
  Call _ a b == Call _ c d = (a, b) == (c, d)
  -- Minor hack to make 'Compose's less in the way.
  Compose _ (V.toList -> [a]) _ == b = a == b
  a == Compose _ (V.toList -> [b]) _ = a == b
  -- 'Compose's are equal regardless of 'StackHint'.
  Compose _ a b == Compose _ c d = (a, b) == (c, d)
  Lambda a b c == Lambda d e f = (a, b, c) == (d, e, f)
  PairTerm a b c == PairTerm d e f = (a, b, c) == (d, e, f)
  Push a b == Push c d = (a, b) == (c, d)
  VectorTerm a b == VectorTerm c d = (a, b) == (c, d)
  _ == _ = False

instance ToText (Term a) where
  toText = \case
    Builtin builtin _ -> toText builtin
    Call _ label _ -> toText label
    Compose _ terms _ -> T.concat
      ["(", T.intercalate " " (V.toList (V.map toText terms)), ")"]
    Lambda name term _ -> T.concat ["(\\", name, " ", toText term, ")"]
    PairTerm a b _ -> T.concat ["(", toText a, ", ", toText b, ")"]
    Push value _ -> toText value
    VectorTerm terms _ -> T.concat
      ["[", T.intercalate ", " (V.toList (V.map toText terms)), "]"]

instance Show (Term a) where
  show = T.unpack . toText

data Value a
  = Bool !Bool !a
  | Char !Char !a
  | Closed !Int !a  -- resolved
  | Closure !(Vector ClosedName) !(Term a) !a  -- resolved
  | Float !Double !a
  | Int !Int !a
  | Local !Int !a  -- resolved
  | String !Text !a
  | Quotation !(Term a) !a
  deriving (Eq)

instance ToText (Value a) where
  toText = \case
    Bool value _ -> if value then "true" else "false"
    Char value _ -> showText value
    Closed index _ -> "closure" <> showText index
    Closure{} -> "<closure>"
    Float value _ -> showText value
    Int value _ -> showText value
    Local index _ -> "local" <> showText index
    String value _ -> toText value
    Quotation term _ -> T.concat ["{", toText term, "}"]

instance Show (Value a) where
  show = T.unpack . toText

type ParsedTerm = Term Location
type ParsedValue = Value Location

type ResolvedTerm = Term Location
type ResolvedValue = Value Location

type TypedTerm = Term (Location, Type Scalar)
type TypedValue = Value (Location, Type Scalar)

defTypeScheme :: Def TypedTerm -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unScheme (defTerm def)

termMetadata :: Term a -> a
termMetadata = \case
  Builtin _ x -> x
  Call _ _ x -> x
  Compose _ _ x -> x
  Lambda _ _ x -> x
  PairTerm _ _ x -> x
  Push _ x -> x
  VectorTerm _ x -> x

typedLocation :: TypedTerm -> Location
typedLocation = fst . termMetadata

typedType :: TypedTerm -> Type Scalar
typedType = snd . termMetadata
