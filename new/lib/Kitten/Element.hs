module Kitten.Element
  ( Element(..)
  ) where

import Kitten.DataDefinition (DataDefinition)
import Kitten.Definition (Definition)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Term (Term)
import Kitten.Trait (Trait)

data Element
  = DataDefinition !DataDefinition
  | Definition !Definition
  | Operator !Operator
  | Synonym !Synonym
  | Term !Term
  | Trait !Trait
