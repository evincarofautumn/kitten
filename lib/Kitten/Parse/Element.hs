{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import qualified Data.Vector as V

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Term

data Element
  = DefElement (Def Term)
  | ImportElement Import
  | TermElement Term

data Partitioned = Partitioned
  { partDefs :: [Def Term]
  , partImports :: [Import]
  , partTerms :: [Term]
  }

partitionElements :: [Element] -> Fragment Term
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

fromPartitioned :: Partitioned -> Fragment Term
fromPartitioned Partitioned{..} = Fragment
  { fragmentDefs = V.fromList partDefs
  , fragmentImports = partImports
  , fragmentTerms = V.fromList partTerms
  }
