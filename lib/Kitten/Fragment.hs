{-# LANGUAGE StandaloneDeriving #-}

module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Monoid
import Data.Vector (Vector)

import Kitten.Def
import Kitten.Import

data Fragment a = Fragment
  { fragmentDefs :: !(Vector (Def a))
  , fragmentImports :: [Import]
  , fragmentTerms :: !(Vector a)
  } deriving (Eq, Show)

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
