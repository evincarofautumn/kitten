{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Control.Arrow

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Operator
import Kitten.Tree

data Element
  = DefElement (Def ParsedTerm)
  | ImportElement Import
  | OperatorElement Operator
  | TermElement ParsedTerm

data Partitioned = Partitioned
  { partDefs :: [Def ParsedTerm]
  , partImports :: [Import]
  , partOperators :: [Operator]
  , partTerms :: [ParsedTerm]
  }

partitionElements :: [Element] -> Fragment ParsedTerm
partitionElements
  = fromPartitioned . foldr go (Partitioned [] [] [] [])
  where
  go element acc = case element of
    DefElement def -> acc
      { partDefs = def : partDefs acc }
    ImportElement import_ -> acc
      { partImports = import_ : partImports acc }
    OperatorElement operator -> acc
      { partOperators = operator : partOperators acc }
    TermElement term -> acc
      { partTerms = term : partTerms acc }

fromPartitioned :: Partitioned -> Fragment ParsedTerm
fromPartitioned Partitioned{..} = Fragment
  { fragmentDefs = H.fromList $ map (defName &&& id) partDefs
  , fragmentImports = partImports
  , fragmentOperators = partOperators
  , fragmentTerms = V.fromList partTerms
  }
