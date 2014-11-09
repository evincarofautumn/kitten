{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse.Element
  ( Element(..)
  , partitionElements
  ) where

import Control.Arrow

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Kitten.Annotation
import Kitten.Definition
import Kitten.Fragment
import Kitten.Import
import Kitten.Location
import Kitten.Operator
import Kitten.Term
import Kitten.Type
import Kitten.TypeDefinition

data Element
  = DefElement (Def ParsedTerm)
  | ImportElement Import
  | OperatorElement Operator
  | TermElement ParsedTerm
  | TypeElement TypeDef

data Partitioned = Partitioned
  { partDefs :: [Def ParsedTerm]
  , partImports :: [Import]
  , partOperators :: [Operator]
  , partTerms :: [ParsedTerm]
  , partTypes :: [TypeDef]
  }

partitionElements :: Location -> [Element] -> Fragment ParsedTerm
partitionElements loc
  = fromPartitioned loc . foldr go (Partitioned [] [] [] [] [])
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
    TypeElement typeDef -> acc
      { partTypes = typeDef : partTypes acc }

fromPartitioned :: Location -> Partitioned -> Fragment ParsedTerm
fromPartitioned loc Partitioned{..} = Fragment
  { fragmentDefs = H.fromList
    $ map (defName &&& id) partDefs
    ++ concatMap
      (\typeDef
        -> map (ctorName &&& defineConstructor typeDef)
          . V.toList $ typeDefConstructors typeDef)
      partTypes
  , fragmentImports = partImports
  , fragmentOperators = partOperators
  , fragmentTerm = TrCompose StackAny (V.fromList partTerms) loc
  , fragmentTypes = H.fromList $ map (typeDefName &&& id) partTypes
  }

-- TODO This desugaring could probably happen somewhere more logical.
defineConstructor :: TypeDef -> TypeConstructor -> Def ParsedTerm
defineConstructor TypeDef{..} TypeConstructor{..} = Def
  { defAnno = Anno
    (AnQuantified V.empty typeDefScalars
      (AnFunction ctorFields
        (V.singleton
          (AnApp (AnVar typeDefName) (V.map AnVar typeDefScalars)))))
    ctorLocation
  , defFixity = Postfix
  , defLocation = ctorLocation
  , defName = ctorName
  , defTerm = mono
    $ TrConstruct typeDefName ctorName (V.length ctorFields) ctorLocation
  }
