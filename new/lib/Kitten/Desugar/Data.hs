module Kitten.Desugar.Data
  ( desugar
  ) where

import Data.List (foldl')
import Kitten.DataConstructor (DataConstructor)
import Kitten.Definition (Definition(Definition))
import Kitten.Fragment (Fragment)
import Kitten.Monad (K)
import Kitten.Name (ConstructorIndex(..), GeneralName(..), Qualified(..), qualifierFromName)
import Kitten.Term (Term(..))
import Kitten.TypeDefinition (TypeDefinition)
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.Definition as Definition
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Operator as Operator
import qualified Kitten.Signature as Signature
import qualified Kitten.TypeDefinition as TypeDefinition

desugar :: Fragment () -> K (Fragment ())
desugar fragment = do
  definitions <- fmap concat $ mapM desugarTypeDefinition
    $ Fragment.types fragment
  return fragment { Fragment.definitions
    = Fragment.definitions fragment ++ definitions }
  where

  desugarTypeDefinition :: TypeDefinition -> K [Definition ()]
  desugarTypeDefinition definition = mapM (uncurry desugarConstructor)
    $ zip [0..] $ TypeDefinition.constructors definition
    where
    desugarConstructor :: Int -> DataConstructor -> K (Definition ())
    desugarConstructor index constructor = do
      let
        resultSignature = foldl'
          (\ a b -> Signature.Application a b origin)
          (Signature.Variable (QualifiedName $ TypeDefinition.name definition)
            $ TypeDefinition.origin definition)
          $ map (\ (parameter, _, parameterOrigin)
            -> Signature.Variable (UnqualifiedName parameter) parameterOrigin)
          $ TypeDefinition.parameters definition
        constructorSignature = Signature.Quantified
          (TypeDefinition.parameters definition)
          (Signature.Function
            (DataConstructor.fields constructor) [resultSignature] [] origin)
          origin
      return Definition
        { Definition.body = New () (ConstructorIndex index)
          $ DataConstructor.origin constructor
        , Definition.category = Definition.Word
        , Definition.fixity = Operator.Postfix
        , Definition.name = Qualified qualifier
          $ DataConstructor.name constructor
        , Definition.origin = origin
        , Definition.signature = constructorSignature
        }
      where
      origin = DataConstructor.origin constructor
      qualifier = qualifierFromName $ TypeDefinition.name definition
