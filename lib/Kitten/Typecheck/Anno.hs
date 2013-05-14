{-# LANGUAGE GADTs #-}

module Kitten.Typecheck.Anno
  ( typecheckAnno
  , typecheckAnnotatedTerms
  ) where

import Control.Monad

import Kitten.Anno (Anno)
import Kitten.Kind
import Kitten.Type
import Kitten.Typecheck.Monad

typecheckAnno :: Anno -> Typecheck
typecheckAnno anno = go $ fromAnno anno
  where
  go :: Type Scalar -> Typecheck
  go type_ = case type_ of
    Composition consumption :> Composition production -> do
      mapM_ popDataExpecting_ $ reverse consumption
      mapM_ pushData production
    AnyType -> unknownTypeError
    AnyType :> AnyType -> unknownTypeError
    AnyType :> Composition{} -> unknownTypeError
    BoolType -> pushData type_
    CharType -> pushData type_
    Composition{} :> AnyType -> unknownTypeError
    FloatType -> pushData type_
    HandleType -> pushData type_
    IntType -> pushData type_
    PairType{} -> pushData type_
    StackFrameType -> internalError
      "stack frames should not appear in annotations"
    UnitType -> pushData type_
    VectorType{} -> pushData type_

typecheckAnnotatedTerms :: Anno -> Typecheck -> Typecheck
typecheckAnnotatedTerms anno action = do
  void . hypothetically $ case type_ of
    AnyType -> unknownTypeError
    AnyType :> AnyType -> unknownTypeError
    AnyType :> Composition{} -> unknownTypeError
    BoolType -> pushData type_
    CharType -> pushData type_
    Composition consumption :> Composition production -> do
      pushData StackFrameType
      mapM_ pushData consumption
      action
      mapM_ popDataExpecting_ $ reverse production
      popDataExpecting_ StackFrameType
    Composition{} :> AnyType -> unknownTypeError
    FloatType -> pushData type_
    HandleType -> pushData type_
    IntType -> pushData type_
    PairType{} -> pushData type_
    StackFrameType -> internalError
      "stack frames should not appear in annotations"
    UnitType -> pushData type_
    VectorType{} -> pushData type_

  pushData type_

  where
  type_ :: Type Scalar
  type_ = fromAnno anno
