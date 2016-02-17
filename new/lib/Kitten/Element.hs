module Kitten.Element
  ( Element(..)
  ) where

import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Term (Term)
import Kitten.Trait (Trait)
import Kitten.TypeDefinition (TypeDefinition)

data Element a
  = Definition !(Definition a)
  | Metadata !Metadata
  | Operator !Operator
  | Synonym !Synonym
  | Term !(Term a)
  | Trait !Trait
  | TypeDefinition !TypeDefinition
