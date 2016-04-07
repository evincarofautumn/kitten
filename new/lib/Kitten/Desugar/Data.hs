module Kitten.Desugar.Data
  ( desugar
  ) where

import Data.List (foldl')
import Kitten.DataConstructor (DataConstructor)
import Kitten.Definition (Definition(Definition))
import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Fragment (Fragment)
import Kitten.Monad (K)
import Kitten.Name (ConstructorIndex(..), GeneralName(..), Qualified(..))
import Kitten.Term (Term(..))
import Kitten.TypeDefinition (TypeDefinition)
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.Definition as Definition
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Entry.Parent as Parent
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
          $ map (\ (Parameter parameterOrigin parameter _kind)
            -> Signature.Variable (UnqualifiedName parameter) parameterOrigin)
          $ TypeDefinition.parameters definition
        constructorSignature = Signature.Quantified
          (TypeDefinition.parameters definition)
          (Signature.Function
            (DataConstructor.fields constructor) [resultSignature] [] origin)
          origin
      return Definition
        { Definition.body = New ()
          (ConstructorIndex index)
          (length $ DataConstructor.fields constructor)
          $ DataConstructor.origin constructor
        , Definition.category = Category.Constructor
        , Definition.fixity = Operator.Postfix
        , Definition.inferSignature = False
        , Definition.merge = Merge.Deny
        , Definition.name = Qualified qualifier
          $ DataConstructor.name constructor
        , Definition.origin = origin
        , Definition.parent = Just $ Parent.Type
          $ TypeDefinition.name definition
        , Definition.signature = constructorSignature
        }
      where
      origin = DataConstructor.origin constructor
      qualifier = qualifierName $ TypeDefinition.name definition
