{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.Resolved
  ( Resolved(..)
  , Value(..)
  ) where

import Data.Vector (Vector)

import qualified Data.Text as T

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def (Def)
import Kitten.Location
import Kitten.Name
import Kitten.Util.Text (Text, ToText(..), showText)

data Resolved
  = Builtin !Builtin !Location
  | Call !Name !Location
  | Compose !(Vector Resolved) !Location
  | PairTerm !Resolved !Resolved !Location
  | Push !Value !Location
  | Scoped !Text !Resolved !Location
  | VectorTerm !(Vector Resolved) !Location
  deriving (Eq, Show)

data Value
  = Bool !Bool
  | Char !Char
  | Closed !Name
  | Closure !(Vector ClosedName) !Resolved
  | Float !Double
  | Function !Resolved
  | Int !Int
  | Local !Name
  | String !Text
  | Unit
  deriving (Eq)

instance AST Resolved where
  type TermValue Resolved = Value
  type TermDef Resolved = Def Resolved

instance Show Value where
  show = T.unpack . toText

instance ToText Value where
  toText value = case value of
    Bool b -> if b then "true" else "false"
    Char c -> showText c
    Closed{} -> "<closed>"
    Closure{} -> "<function>"
    Float f -> showText f
    Function{} -> "<function>"
    Int i -> showText i
    Local{} -> "<local>"
    String s -> s
    Unit -> "()"
