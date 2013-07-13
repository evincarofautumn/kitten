{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Scheme
  ( free
  , generalize
  , instantiate
  , instantiateM
  , normalize
  , occurs
  , sub
  , subChain
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type

instantiateM :: Scheme -> Inferred Type
instantiateM = Inferred . lift . state . instantiate

instantiate :: Scheme -> Env -> (Type, Env)
instantiate (Forall names type_) env
  = let (renamedEnv, env') = foldr rename (emptyEnv, env) (Set.toList names)
  in (sub renamedEnv type_, env')
  where
  rename :: Name -> (Env, Env) -> (Env, Env)
  rename name (localEnv, globalEnv)
    = let (var, globalEnv') = freshVar globalEnv
    in (declare name var localEnv, globalEnv')

generalize :: Inferred Type -> Inferred Scheme
generalize action = do
  before <- getEnv
  type_ <- action
  after <- getEnv
  let
    substituted = sub after type_
    dependent = dependentBetween before after
    names = filter dependent $ free substituted
  return $ Forall (Set.fromList names) substituted

free :: Type -> [Name]
free = nub . free'
  where
  free' :: Type -> [Name]
  free' type_ = case type_ of
    a :& b -> free a ++ free b
    FunctionType a b _ -> concatMap free' a ++ concatMap free' b
    BoolType -> []
    CharType -> []
    FloatType -> []
    GeneratedType -> []
    HandleType -> []
    IntType -> []
    TestType -> []
    TypeVar name -> [name]
    UnitType -> []
    VectorType a -> free' a

normalize :: Type -> Type
normalize type_ = let
  names = free type_
  env = emptyEnv
    { envTypes = Map.fromList
      $ zip names (map var [0..]) }
  in sub env type_
  where
  var :: Int -> Type
  var = TypeVar . Name

occurs :: Name -> Env -> Type -> Bool
occurs = (((> 0) .) .) . occurrences

occurrences :: Name -> Env -> Type -> Int
occurrences name env type_ = case type_ of
  a :& b -> occurrences name env a + occurrences name env b
  FunctionType a b _
    -> sum (map (occurrences name env) a)
    + sum (map (occurrences name env) b)
  BoolType -> 0
  CharType -> 0
  FloatType -> 0
  GeneratedType -> 0
  HandleType -> 0
  IntType -> 0
  TestType -> 0
  TypeVar name' -> case findType env name' of
    Left{} -> if name == name' then 1 else 0
    Right type' -> occurrences name env type'
  UnitType -> 0
  VectorType a -> occurrences name env a

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween :: Env -> Env -> Name -> Bool
dependentBetween before after name
  = any (bound after) (unbound before)
  where
  bound env name' = occurs name env (TypeVar name')

-- | Enumerates those type variables in an environment which
-- are allocated but not yet bound to a type.
unbound :: Env -> [Name]
unbound Env{..} = filter
  (not . (`Map.member` envTypes))
  [Name 0 .. pred envNext]

subChain :: Env -> Type -> Type
subChain env type_ = case type_ of
  TypeVar name
    | Right type' <- findType env name
    -> subChain env type'
  _ -> type_

sub :: Env -> Type -> Type
sub env type_ = case type_ of
  FunctionType a b p -> FunctionType (map (sub env) a) (map (sub env) b) p
  a :& b -> sub env a :& sub env b
  BoolType{} -> type_
  CharType{} -> type_
  FloatType{} -> type_
  GeneratedType{} -> type_
  IntType{} -> type_
  HandleType{} -> type_
  TestType -> type_
  TypeVar name
    | Right type' <- findType env name
    -> sub env type'
    | otherwise
    -> type_
  UnitType{} -> type_
  VectorType a -> VectorType (sub env a)
