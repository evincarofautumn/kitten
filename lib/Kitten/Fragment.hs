module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Monoid

import Kitten.Def
import Kitten.Import
import Kitten.TypeDef

data Fragment a b = Fragment
  { fragmentDefs :: [Def a]
  , fragmentImports :: [Import]
  , fragmentTerms :: [b]
  , fragmentTypeDefs :: [TypeDef]
  } deriving (Eq, Show)

instance Monoid (Fragment a b) where
  mempty = Fragment
    { fragmentDefs = []
    , fragmentImports = []
    , fragmentTerms = []
    , fragmentTypeDefs = []
    }
  mappend a b = Fragment
    { fragmentDefs = fragmentDefs a ++ fragmentDefs b
    , fragmentImports = fragmentImports a ++ fragmentImports b
    , fragmentTerms = fragmentTerms a ++ fragmentTerms b
    , fragmentTypeDefs = fragmentTypeDefs a ++ fragmentTypeDefs b
    }
