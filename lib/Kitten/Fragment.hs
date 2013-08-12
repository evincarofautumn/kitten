module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Monoid
import Data.Vector (Vector)

import Kitten.Def
import Kitten.Import
import Kitten.TypeDef

data Fragment a b = Fragment
  { fragmentDefs :: !(Vector (Def a))
  , fragmentImports :: !(Vector Import)
  , fragmentTerms :: !(Vector b)
  , fragmentTypeDefs :: !(Vector TypeDef)
  } deriving (Eq, Show)

instance Monoid (Fragment a b) where
  mempty = Fragment
    { fragmentDefs = mempty
    , fragmentImports = mempty
    , fragmentTerms = mempty
    , fragmentTypeDefs = mempty
    }
  mappend a b = Fragment
    { fragmentDefs = fragmentDefs a <> fragmentDefs b
    , fragmentImports = fragmentImports a <> fragmentImports b
    , fragmentTerms = fragmentTerms a <> fragmentTerms b
    , fragmentTypeDefs = fragmentTypeDefs a <> fragmentTypeDefs b
    }
