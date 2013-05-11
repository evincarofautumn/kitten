module Kitten.Typecheck.Manifest
  ( manifestTermType
  , manifestValueType
  ) where

import Kitten.Kind
import Kitten.Resolved
import Kitten.Type
import Kitten.Typecheck.Monad

manifestTermType :: Resolved -> TypecheckM (Type Scalar)
manifestTermType resolved = case resolved of
  Push value _ -> manifestValueType value
  _ -> internalError "TODO manifest term type of non-function"

manifestValueType :: Value -> TypecheckM (Type Scalar)
manifestValueType value = case value of
  Bool _ -> return BoolType
  Char _ -> return CharType
  Float _ -> return FloatType
  Function anno _ -> return $ fromAnno anno
  Closure anno _ _ -> return $ fromAnno anno
  Int _ -> return IntType
  Vector (Just anno) _ -> return . VectorType $ fromAnno anno
  _ -> internalError
   $ "manifest value type of non-literal: " ++ show value
