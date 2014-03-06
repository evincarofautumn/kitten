{-# LANGUAGE StandaloneDeriving #-}

module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Monoid
import Data.Vector (Vector)

import Kitten.AST
import Kitten.Import

data Fragment a = Fragment
  { fragmentDefs :: !(Vector (TermDef a))
  , fragmentImports :: !(Vector Import)
  , fragmentTerms :: !(Vector a)
  }

deriving instance (AST a) => Eq (Fragment a)
deriving instance (AST a) => Show (Fragment a)

instance Monoid (Fragment a) where
  mempty = Fragment
    { fragmentDefs = mempty
    , fragmentImports = mempty
    , fragmentTerms = mempty
    }
  mappend a b = Fragment
    { fragmentDefs = fragmentDefs a <> fragmentDefs b
    , fragmentImports = fragmentImports a <> fragmentImports b
    , fragmentTerms = fragmentTerms a <> fragmentTerms b
    }
