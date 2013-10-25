{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.Typed
  ( Typed(..)
  , TypedDef
  , Value(..)
  , defTypeScheme
  , typedType
  ) where

import Control.Applicative ((<$))
import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Location
import Kitten.Name
import Kitten.Type
import Kitten.Util.Text (ToText(..), showText)

data Typed
  = Builtin !Builtin !Location (Type Scalar)
  | Call !Name !Location (Type Scalar)
  | Compose !(Vector Typed) !Location (Type Scalar)
  | From !Text !Location (Type Scalar)
  | PairTerm !Typed !Typed !Location (Type Scalar)
  | Push !Value !Location (Type Scalar)
  | To !Text !Location (Type Scalar)
  | Scoped !Typed !Location (Type Scalar)
  | VectorTerm !(Vector Typed) !Location (Type Scalar)
  deriving (Eq, Show)

-- TODO(strager)
instance ToText Typed where
  toText = showText

data Value
  = Bool !Bool
  | Char !Char
  | Closed !Name
  | Closure !(Vector ClosedName) !Typed
  | Float !Double
  | Int !Int
  | Local !Name
  | Unit
  | String !Text
  deriving (Eq, Show)

type TypedDef = Def (Scheme Typed)

instance AST Typed where
  type TermValue Typed = Value
  type TermDef Typed = TypedDef

defTypeScheme :: TypedDef -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unScheme (defTerm def)

typedType :: Typed -> Type Scalar
typedType typed = case typed of
  Builtin _ _ t -> t
  Call _ _ t -> t
  Compose _ _ t -> t
  From _ _ t -> t
  PairTerm _ _ _ t -> t
  Push _ _ t -> t
  To _ _ t -> t
  Scoped _ _ t -> t
  VectorTerm _ _ t -> t
