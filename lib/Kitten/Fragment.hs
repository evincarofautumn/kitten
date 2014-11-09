module Kitten.Fragment where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import qualified Data.HashMap.Strict as H

import Kitten.Definition
import Kitten.Import
import Kitten.Operator
import Kitten.TypeDefinition

data Fragment a = Fragment
  { fragmentDefs :: !(HashMap Text (Def a))
  , fragmentImports :: [Import]
  , fragmentOperators :: [Operator]
  , fragmentTerm :: !a
  , fragmentTypes :: !(HashMap Text TypeDef)
  } deriving (Eq, Show)

termFragment :: a -> Fragment a
termFragment term = Fragment
  { fragmentDefs = H.empty
  , fragmentImports = []
  , fragmentOperators = []
  , fragmentTerm = term
  , fragmentTypes = H.empty
  }
