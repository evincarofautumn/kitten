{-# LANGUAGE TypeFamilies #-}

module Kitten.Typed
  ( Typed(..)
  , Value(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Type

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

data Value
  = Bool !Bool
  | Char !Char
  | Closed !Name
  | Closure !(Vector ClosedName) !Typed
  | Float !Double
  | Function !Typed
  | Int !Int
  | Local !Name
  | Unit
  | Vector !(Vector Value)
  deriving (Eq, Show)

instance AST Typed where
  type TermValue Typed = Value
