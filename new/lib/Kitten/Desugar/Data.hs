module Kitten.Desugar.Data
  ( desugar
  ) where

import Data.List (foldl')
import Kitten.DataConstructor (DataConstructor)
import Kitten.DataDefinition (DataDefinition)
import Kitten.Definition (Definition(Definition))
import Kitten.Fragment (Fragment)
import Kitten.Monad (K)
import Kitten.Name (ConstructorIndex(..), GeneralName(..), Qualified(..), qualifierFromName)
import Kitten.Term (Term(..))
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.DataDefinition as DataDefinition
import qualified Kitten.Definition as Definition
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Operator as Operator
import qualified Kitten.Signature as Signature

desugar :: Fragment -> K Fragment
desugar fragment = do
  definitions <- fmap concat $ mapM desugarDataDefinition
    $ Fragment.dataDefinitions fragment
  return fragment { Fragment.definitions
    = Fragment.definitions fragment ++ definitions }
  where

  desugarDataDefinition :: DataDefinition -> K [Definition]
  desugarDataDefinition definition = mapM (uncurry desugarConstructor)
    $ zip [0..] $ DataDefinition.constructors definition
    where
    desugarConstructor :: Int -> DataConstructor -> K Definition
    desugarConstructor index constructor = do
      let
        resultSignature = foldl'
          (\ a b -> Signature.Application a b origin)
          (Signature.Variable (QualifiedName $ DataDefinition.name definition)
            $ DataDefinition.origin definition)
          $ map (\ (parameter, _, parameterOrigin)
            -> Signature.Variable (UnqualifiedName parameter) parameterOrigin)
          $ DataDefinition.parameters definition
        constructorSignature = Signature.Quantified
          (DataDefinition.parameters definition)
          (Signature.Function
            (DataConstructor.fields constructor) [resultSignature] [] origin)
          origin
      return Definition
        { Definition.body = New Nothing (ConstructorIndex index)
          $ DataConstructor.origin constructor
        , Definition.fixity = Operator.Postfix
        , Definition.mangling = Definition.Word
        , Definition.name = Qualified qualifier
          $ DataConstructor.name constructor
        , Definition.origin = origin
        , Definition.signature = constructorSignature
        }
      where
      origin = DataConstructor.origin constructor
      qualifier = qualifierFromName $ DataDefinition.name definition
