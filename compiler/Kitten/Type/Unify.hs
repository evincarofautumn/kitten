{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type.Unify
  ( unify
  , unifyM
  , unifyM_
  , unifyVar
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.Monoid

import qualified Data.Text as Text

import Kitten.Error
import Kitten.Name
import Kitten.Type
import Kitten.Type.Inference
import Kitten.Type.Substitute
import Kitten.Util

-- | Simplifies and unifies two types.
unify :: Type -> Type -> Env -> Either CompileError Env
unify a b env = (unify' `on` substChain env) a b env

-- | Unifies two simplified types.
unify' :: Type -> Type -> Env -> Either CompileError Env
unify' a b | a == b = return
unify' (VecType a) (VecType b) = unify a b
unify' (a :> b) (c :> d) = unify b d >=> unify a c
unify' (a :. b) (c :. d) = unify b d >=> unify a c
unify' (Var var) type_ = unifyVar var type_
unify' type_ (Var var) = unifyVar var type_
unify' a b = const . Left . CompileError $ Text.unwords
  [ "cannot solve type constraint:"
  , textShow a
  , "="
  , textShow b
  ]

-- | Unifies two types, returning the second type.
unifyM :: Type -> Type -> Inference Type
unifyM type1 type2 = do
  env <- gets $ unify type1 type2
  case env of
    Right env' -> put env' >> return type2
    Left err -> lift . Left . CompileError
      $ "Unification error: " <> textShow err

-- | Unifies two types.
unifyM_ :: Type -> Type -> Inference ()
unifyM_ = (void .) . unifyM

-- | Unifies a variable with a type.
unifyVar :: Name -> Type -> Env -> Either CompileError Env
unifyVar var1 (Var var2) env | var1 == var2 = return env
unifyVar var1 (Var var2) env
  = return $ declareType var1 (Var var2) env
unifyVar var type_ env | occurs var type_ env
  = Left . CompileError $ Text.unwords
    ["cannot construct infinite type", varText, "=", typeText]
  where
  varText = textShow $ substType env (Var var)
  typeText = textShow $ substType env type_
unifyVar var type_ env
  = return $ declareType var type_ env
