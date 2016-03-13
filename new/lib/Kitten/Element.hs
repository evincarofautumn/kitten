module Kitten.Element
  ( Element(..)
  ) where

import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Term (Term)
import Kitten.TypeDefinition (TypeDefinition)

data Element a
  = Declaration !Declaration
  | Definition !(Definition a)
  | Metadata !Metadata
  | Operator !Operator
  | Synonym !Synonym
  | Term !(Term a)
  | TypeDefinition !TypeDefinition
