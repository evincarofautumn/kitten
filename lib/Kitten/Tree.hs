{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
  , typedType
  ) where

import Control.Applicative ((<$))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T

import Kitten.Builtin
import Kitten.ClosedName
import Kitten.Def
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Type hiding (Annotated(..), Hint(..))
import Kitten.Util.Text (ToText(..))

-- TODO Add pretty 'Show' instance for 'Term's.
data Term label a
  = Builtin !Builtin !a
  | Call FixityHint !label !a
  | Compose !(Vector (Term label a)) !a
  | Lambda !Text !(Term label a) !a
  | PairTerm !(Term label a) !(Term label a) !a
  | Push !(Value label a) !a
  | UnparsedApplicative [Term label a] !a
  | VectorTerm !(Vector (Term label a)) !a
  deriving (Show)

instance (Eq label, Eq a) => Eq (Term label a) where
  Builtin a b == Builtin c d = (a, b) == (c, d)
  -- Calls are equal regardless of 'FixityHint'.
  Call _ a b == Call _ c d = (a, b) == (c, d)
  Compose a b == Compose c d = (a, b) == (c, d)
  Lambda a b c == Lambda d e f = (a, b, c) == (d, e, f)
  PairTerm a b c == PairTerm d e f = (a, b, c) == (d, e, f)
  Push a b == Push c d = (a, b) == (c, d)
  UnparsedApplicative a b == UnparsedApplicative c d = (a, b) == (c, d)
  VectorTerm a b == VectorTerm c d = (a, b) == (c, d)
  _ == _ = False

instance (Show label, Show a) => ToText (Term label a) where
  toText = T.pack . show

data Value label a
  = Bool !Bool !a
  | Char !Char !a
  | Closed !Name !a  -- resolved
  | Closure !(Vector ClosedName) !(Term label a) !a  -- resolved
  | Float !Double !a
  | Function !(Term label a) !a
  | Int !Int !a
  | Local !Name !a  -- resolved
  | Unit !a
  | String !Text !a
  deriving (Eq, Show)

instance (Show label, Show a) => ToText (Value label a) where
  toText = T.pack . show

type ParsedTerm = Term Text Location
type ParsedValue = Value Text Location

type ResolvedTerm = Term Name Location
type ResolvedValue = Value Name Location

type TypedTerm = Term Name (Location, Type Scalar)
type TypedValue = Value Name (Location, Type Scalar)

defTypeScheme :: Def TypedTerm -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unScheme (defTerm def)

termMetadata :: Term label a -> a
termMetadata = \case
  Builtin _ x -> x
  Call _ _ x -> x
  Compose _ x -> x
  Lambda _ _ x -> x
  PairTerm _ _ x -> x
  Push _ x -> x
  UnparsedApplicative _ x -> x
  VectorTerm _ x -> x

typedType :: TypedTerm -> Type Scalar
typedType = snd . termMetadata
