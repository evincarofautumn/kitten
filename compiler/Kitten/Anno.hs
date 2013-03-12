module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Name

data Anno = Anno
  { annoName :: !Text
  , annoVars :: !(Set Name)
  , annoType :: !Type
  }

data Type
  = !Type :> !Type
  | !Type :. !Type
  | Vec !Type
  | Tuple !(Vector Type)
  | Empty
  | Var !Name
  | Word !Text
