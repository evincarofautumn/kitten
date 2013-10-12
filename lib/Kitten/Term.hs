{-# LANGUAGE TypeFamilies #-}

module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.Def (Def)
import Kitten.Location

data Term
  = Builtin !Builtin !Location
  | Call !Text !Location
  | Compose !(Vector Term) !Location
  | From !Text !Location
  | Lambda !Text !Term !Location
  | PairTerm !Term !Term !Location
  | Push !Value !Location
  | To !Text !Location
  | VectorTerm !(Vector Term) !Location
  deriving (Eq, Show)

data Value
  = Bool !Bool !Location
  | Char !Char !Location
  | Float !Double !Location
  | Function !(Vector Term) !Location
  | Int !Int !Location
  | Unit !Location
  | String !Text !Location
  deriving (Eq, Show)

instance AST Term where
  type TermValue Term = Value
  type TermDef Term = Def Value
