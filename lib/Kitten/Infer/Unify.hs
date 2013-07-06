module Kitten.Infer.Unify
  ( unify
  , unifyM
  , unifyM_
  , unifyRow
  , unifyRowM
  , unifyVar
  ) where

import Control.Monad
import Data.Foldable (foldrM)
import Data.Function

import Kitten.Error
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Name
import Kitten.Type
import Kitten.Util.FailWriter
import Kitten.Util.Show

-- | Simplifies and unifies two types.
unify :: Type -> Type -> Env -> Either CompileError Env
unify a b env = (unify' `on` subChain env) a b env

-- | Unifies two simplified types.
unify' :: Type -> Type -> Env -> Either CompileError Env
unify' type1 type2 env = case (type1, type2) of
  _ | type1 == type2 -> Right env

  (VectorType a, VectorType b) -> unify a b env

  (a :> b, c :> d) -> unifyRow b d env >>= unifyRow a c

  (TypeVar var, type_) -> unifyVar var type_ env
  (type_, TypeVar var) -> unifyVar var type_ env

  _ -> Left . TypeError (envLocation env) $ unwords
    [ "cannot solve scalar type constraint:"
    , show type1
    , "="
    , show type2
    ]

-- | Unifies two types, returning the second type.
unifyM :: Type -> Type -> Inferred Type
unifyM type1 type2 = do
  env <- getsEnv $ unify type1 type2
  case env of
    Right env' -> putEnv env' >> return type2
    Left err -> Inferred $ throwMany [err]

unifyM_ :: Type -> Type -> Inferred ()
unifyM_ = (void .) . unifyM

unifyVar :: Name -> Type -> Env -> Either CompileError Env
unifyVar var1 type_ env = case type_ of
  TypeVar var2 | var1 == var2 -> return env
  TypeVar{} -> return $ declare var1 type_ env
  _ | occurs var1 env type_
    -> Left . TypeError (envLocation env) $ unwords
      [ "cannot construct infinite type"
      , show $ sub env (TypeVar var1)
      , "="
      , show $ sub env type_
      , "\n"
      , show env
      ]
  _ -> return $ declare var1 type_ env

unifyRow :: [Type] -> [Type] -> Env -> Either CompileError Env
unifyRow as bs env = if length as /= length bs
  then Left $ TypeError (envLocation env) $ unwords
    [ "cannot solve row type constraint:"
    , showWords $ map (sub env) as
    , "="
    , showWords $ map (sub env) bs
    ]
  else foldrM (uncurry unify) env $ zip as bs

unifyRowM :: [Type] -> [Type] -> Inferred ()
unifyRowM a b = do
  env <- getsEnv $ unifyRow a b
  case env of
    Right env' -> putEnv env'
    Left err -> Inferred $ throwMany [err]
