{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type.Inference
  ( Env(..)
  , Inference
  , declareType
  , emptyEnv
  , fresh
  , freshFunction
  , findType
  , occurs
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.IntMap (IntMap)
import Data.Monoid

import qualified Data.IntMap as IntMap
import qualified Data.Vector as Vector

import Kitten.Error
import Kitten.Name
import Kitten.Type
import Kitten.Util

data Env = Env
  { envNext  :: Name
  , envTypes :: IntMap Type
  , envLocals :: [TypeScheme]
  , envInstantiations :: [(Type, Name)]
  }

type Inference = StateT Env (Either CompileError)

declareType :: Name -> Type -> Env -> Env
declareType (Name var) type_ env@Env{..}
  = env { envTypes = IntMap.insert var type_ envTypes }

emptyEnv :: Env
emptyEnv = Env (Name 0) IntMap.empty [] []

fresh :: Inference Type
fresh = Var <$> freshName
  where
  freshName = do
    name@(Name x) <- gets envNext
    modify $ \ env -> env { envNext = Name $ succ x }
    return name

freshFunction :: Inference Type
freshFunction = (:>) <$> fresh <*> fresh

findType :: Env -> Name -> Either CompileError Type
findType Env{..} (Name var) = maybeToEither
  (CompileError $ "Nonexistent type variable " <> textShow var <> "!")
  $ IntMap.lookup var envTypes

occurs :: Name -> Type -> Env -> Bool
occurs _ IntType _ = False
occurs _ BoolType _ = False
occurs _ TextType _ = False
occurs _ EmptyType _ = False
occurs var (SVec type_ _) env = occurs var type_ env
occurs var (DVec type_) env = occurs var type_ env
occurs var (TupleType types) env
  = Vector.any (\ type_ -> occurs var type_ env) types
occurs var1 (Var var2) env = case findType env var2 of
  Left _ -> var1 == var2
  Right type_ -> occurs var1 type_ env
occurs var (a :> b) env = occurs var a env || occurs var b env
occurs var (a :. b) env = occurs var a env || occurs var b env
