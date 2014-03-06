{-# LANGUAGE DataKinds #-}

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
import Kitten.Type hiding (Annotated(..), Hint(..))
import Kitten.Util.Text (ToText(..))

data Term label a
  = Builtin !Builtin !a
  | Call !label !a
  | Compose !(Vector (Term label a)) !a
  | Lambda !Text !(Term label a) !a
  | PairTerm !(Term label a) !(Term label a) !a
  | Push !(Value label a) !a
  | VectorTerm !(Vector (Term label a)) !a
  deriving (Eq, Show)

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

typedType :: TypedTerm -> Type Scalar
typedType typed = case typed of
  Builtin _ (_, t) -> t
  Call _ (_, t) -> t
  Compose _ (_, t) -> t
  Lambda _ _ (_, t) -> t
  PairTerm _ _ (_, t) -> t
  Push _ (_, t) -> t
  VectorTerm _ (_, t) -> t
