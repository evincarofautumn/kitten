module Kitten.TypeDefinition where

import Data.Vector (Vector)

import Kitten.Annotation
import Kitten.Location
import Kitten.Name

data TypeDef = TypeDef
  { typeDefConstructors :: !(Vector TypeConstructor)
  , typeDefLocation :: !Location
  , typeDefName :: !Name
  , typeDefScalars :: !(Vector Name)
  } deriving (Eq, Show)

data TypeConstructor = TypeConstructor
  { ctorFields :: !(Vector AnType)
  , ctorLocation :: !Location
  , ctorName :: !Name
  } deriving (Eq, Show)
