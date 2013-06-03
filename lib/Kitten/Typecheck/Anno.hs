{-# LANGUAGE GADTs #-}

module Kitten.Typecheck.Anno
  ( typecheckAnno
  ) where

import Kitten.Anno (Anno)
import Kitten.Kind
import Kitten.Type
import Kitten.Typecheck.Monad

typecheckAnno :: Anno -> Typecheck ()
typecheckAnno anno = go $ fromAnno anno
  where
  go :: Type Scalar -> Typecheck ()
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
