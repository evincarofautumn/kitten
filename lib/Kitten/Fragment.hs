module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Monoid

import Kitten.Def
import Kitten.Import

data Fragment a b = Fragment
  { fragmentDefs :: [Def a]
  , fragmentImports :: [Import]
  , fragmentTerms :: [b]
  } deriving (Eq, Show)

instance Monoid (Fragment a b) where
  mempty = Fragment
    { fragmentDefs = []
    , fragmentImports = []
    , fragmentTerms = []
    }
  mappend a b = Fragment
    { fragmentDefs = fragmentDefs a ++ fragmentDefs b
    , fragmentImports = fragmentImports a ++ fragmentImports b
    , fragmentTerms = fragmentTerms a ++ fragmentTerms b
    }
