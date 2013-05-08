module Kitten.Typecheck.Manifest
  ( manifestTermType
  , manifestValueType
  ) where

import Kitten.Kind
import Kitten.Resolve
import Kitten.Type
import Kitten.Typecheck.Monad

manifestTermType :: Resolved -> TypecheckM (Type Scalar)
manifestTermType resolved = case resolved of
  Push value _ -> manifestValueType value
  _ -> internalError "TODO manifest term type of non-function"

manifestValueType :: Value -> TypecheckM (Type Scalar)
manifestValueType value = case value of
  Bool _ -> return BoolType
  Float _ -> return FloatType
  Function anno _ -> return $ fromAnno anno
  Int _ -> return IntType
  Text _ -> return TextType
  Vector (Just anno) _ -> return . VectorType $ fromAnno anno
  _ -> internalError "TODO manifest value type of non-literal"
