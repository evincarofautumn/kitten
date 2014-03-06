{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import qualified Data.Vector as V

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Tree

data Element
  = DefElement (Def ParsedTerm)
  | ImportElement Import
  | TermElement ParsedTerm

data Partitioned = Partitioned
  { partDefs :: [Def ParsedTerm]
  , partImports :: [Import]
  , partTerms :: [ParsedTerm]
  }

partitionElements :: [Element] -> Fragment ParsedTerm
partitionElements
  = fromPartitioned . foldr go (Partitioned [] [] [])
  where
  go element acc = case element of
    DefElement def -> acc
      { partDefs = def : partDefs acc }
    ImportElement import_ -> acc
      { partImports = import_ : partImports acc }
    TermElement term -> acc
      { partTerms = term : partTerms acc }

fromPartitioned :: Partitioned -> Fragment ParsedTerm
fromPartitioned Partitioned{..} = Fragment
  { fragmentDefs = V.fromList partDefs
  , fragmentImports = partImports
  , fragmentTerms = V.fromList partTerms
  }
