module Kitten.TypeDefinition where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Annotation
import Kitten.Location

data TypeDef = TypeDef
  { typeDefConstructors :: !(Vector TypeConstructor)
  , typeDefLocation :: !Location
  , typeDefName :: !Text
  , typeDefScalars :: !(Vector Text)
  } deriving (Eq, Show)

data TypeConstructor = TypeConstructor
  { ctorFields :: !(Vector AnType)
  , ctorLocation :: !Location
  , ctorName :: !Text
  } deriving (Eq, Show)
