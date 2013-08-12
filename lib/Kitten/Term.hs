module Kitten.Term
  ( Term(..)
  , Value(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Builtin (Builtin)
import Kitten.Location

data Term
  = Builtin !Builtin !Location
  | Call !Text !Location
  | ChoiceTerm !Term !Term !Location
  | Compose !(Vector Term) !Location
  | From !Text !Location
  | Group !(Vector Term) !Location
  | If !Term !Term !Location
  | Lambda !Text !Term !Location
  | OptionTerm !Term !Term !Location
  | PairTerm !Term !Term !Location
  | Push !Value !Location
  | To !Text !Location
  | VectorTerm !(Vector Term) !Location
  deriving (Eq, Show)

data Value
  = Bool !Bool !Location
  | Char !Char !Location
  | Choice !Bool !Value !Location
  | Float !Double !Location
  | Function !(Vector Term) !Location
  | Int !Int !Location
  | Option !(Maybe Value) !Location
  | Pair !Value !Value !Location
  | Unit !Location
  | Vector !(Vector Value) !Location
  deriving (Eq, Show)
