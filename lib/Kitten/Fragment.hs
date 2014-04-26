{-# LANGUAGE StandaloneDeriving #-}

module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Def
import Kitten.Import
import Kitten.Operator

data Fragment a = Fragment
  { fragmentDefs :: !(HashMap Text (Def a))
  , fragmentImports :: [Import]
  , fragmentOperators :: [Operator]
  , fragmentTerms :: !(Vector a)
  } deriving (Eq, Show)

instance Monoid (Fragment a) where
  mempty = Fragment
    { fragmentDefs = mempty
    , fragmentImports = mempty
    , fragmentOperators = mempty
    , fragmentTerms = mempty
    }
  mappend a b = Fragment
    { fragmentDefs = fragmentDefs a <> fragmentDefs b
    , fragmentImports = fragmentImports a <> fragmentImports b
    , fragmentOperators = fragmentOperators a <> fragmentOperators b
    , fragmentTerms = fragmentTerms a <> fragmentTerms b
    }
