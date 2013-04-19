module Kitten.Anno
  ( Anno(..)
  , Type(..)
  ) where

import Data.Set (Set)

import Kitten.Name

data Anno = Anno
  { annoName :: String
  , annoVars :: Set Name
  , annoType :: Type
  } deriving (Eq, Show)

data Type
  = Type :> Type
  | Type :. Type
  | Vec Type
  | Tuple [Type]
  | Empty
  | Var Name
  | Word String
  deriving (Eq, Show)
