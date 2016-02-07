module Kitten.Element
  ( Element(..)
  ) where

import Kitten.DataDefinition (DataDefinition)
import Kitten.Definition (Definition)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Term (Term)
import Kitten.Trait (Trait)

data Element a
  = DataDefinition !DataDefinition
  | Definition !(Definition a)
  | Operator !Operator
  | Synonym !Synonym
  | Term !(Term a)
  | Trait !Trait
