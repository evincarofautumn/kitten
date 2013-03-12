module Kitten.Anno
  ( Anno(..)
  , Sig(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Name

data Anno a = Anno
  { annoName :: !a
  , annoVars :: !(Set Name)
  , annoSig :: !Sig
  } deriving (Show)

data Sig
  = Function !Sig !Sig
  | Compose !Sig !Sig
  | Vec !Sig
  | Tuple !(Vector Sig)
  | Empty
  | Var !Name
  | Word !Text
  deriving (Show)
