module Kitten.Type.Scheme
  ( generalize
  , instantiate
  , makeInstantiation
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Set (Set)

import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Kitten.Name
import Kitten.Type
import Kitten.Type.Inference
import Kitten.Type.Substitute
import Kitten.Util

-- | Makes a reference to the type scheme of a definition
-- which will be instantiated later.
makeInstantiation :: Name -> Inference Type
makeInstantiation name = do
  type_ <- freshFunction
  modify $ \ env@Env{..} -> env
    { envInstantiations = (type_, name) : envInstantiations }
  return type_

-- | Instantiates a type scheme in the current environment.
instantiate :: TypeScheme -> Inference Type
instantiate (Forall vars type_) = do
  env <- Foldable.foldrM rename emptyEnv vars
  return $ substType env type_
  where rename v env = declareType v <$> fresh <*> pure env

-- | Generalizes a type into a type scheme.
generalize :: Inference Type -> Inference TypeScheme
generalize action = do
  before <- get
  type_ <- action
  after <- get
  let
    substituted = substType after type_
    dependent = dependentBetween before after
    vars = Set.filter dependent $ free substituted
  return $ Forall vars substituted

-- | Enumerates free variables of a type.
free :: Type -> Set Name
free IntType = mempty
free BoolType = mempty
free TextType = mempty
free (a :> b) = free a <> free b
free (a :. b) = free a <> free b
free (VecType a) = free a
free (TupleType types) = mconcatMap free $ Vector.toList types
free (Var var) = Set.singleton var
free EmptyType = mempty

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween :: Env -> Env -> Name -> Bool
dependentBetween before after var1
  = any (bound after) (unbound before)
  where bound env var2 = occurs var1 (Var var2) env

-- | Enumerates those type variables in an environment which
-- are allocated but not yet bound to a type.
unbound :: Env -> [Name]
unbound Env{..} = filter (\ (Name var) -> not $ IntMap.member var envTypes)
  [Name 0 .. pred envNext]
