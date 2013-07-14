{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Infer.Scheme
  ( Occurrences(..)
  , Simplify(..)
  , Substitute(..)
  , free
  , generalize
  , instantiate
  , instantiateM
  , normalize
  , occurs
  ) where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type

instantiateM :: Scheme -> Inferred (Type Scalar)
instantiateM = Inferred . lift . state . instantiate

instantiate :: Scheme -> Env -> (Type Scalar, Env)
instantiate (Forall rows scalars type_) env = let
  (renamedEnv, env') = foldr rename (emptyEnv, env) (Set.toList rows)
  (renamedEnv', env'') = foldr rename (renamedEnv, env') (Set.toList scalars)
  in (sub renamedEnv' type_, env'')
  where
  rename :: (Declare a) => TypeName a -> (Env, Env) -> (Env, Env)
  rename name (localEnv, globalEnv)
    = let (var, globalEnv') = freshVar globalEnv
    in (declare name var localEnv, globalEnv')

generalize :: Inferred (Type Scalar) -> Inferred Scheme
generalize action = do
  before <- getEnv
  type_ <- action
  after <- getEnv

  let
    substituted :: Type Scalar
    substituted = sub after type_

    dependent :: (Occurrences a) => TypeName a -> Bool
    dependent = dependentBetween before after

    rows :: [TypeName Row]
    scalars :: [TypeName Scalar]
    (rows, scalars) = filter dependent *** filter dependent
      $ freeVars substituted

  return $ Forall
    (Set.fromList rows)
    (Set.fromList scalars)
    substituted

freeVars :: (Free a) => Type a -> ([TypeName Row], [TypeName Scalar])
freeVars type_
  = let (rows, scalars) = free type_
  in (nub rows, nub scalars)

class Free a where
  free :: Type a -> ([TypeName Row], [TypeName Scalar])

instance Free Row where
  free type_ = case type_ of
    a :. b -> free a <> free b
    Empty -> mempty
    Test -> mempty
    Var name -> ([row name], [])

instance Free Scalar where
  free type_ = case type_ of
    a :& b -> free a <> free b
    (:?) a -> free a
    a :| b -> free a <> free b
    Function a b _ -> free a <> free b
    Bool -> mempty
    Char -> mempty
    Float -> mempty
    Handle -> mempty
    Int -> mempty
    Test -> mempty
    Var name -> ([], [scalar name])
    Unit -> mempty
    Vector a -> free a

normalize :: Type Scalar -> Type Scalar
normalize type_ = let
  (rows, scalars) = freeVars type_
  env = emptyEnv
    { envRows = Map.fromList
      $ zip (map typeName rows) (map var [0..])
    , envScalars = Map.fromList
      $ zip (map typeName scalars) (map var [length rows..])
    }
  in sub env type_
  where
  var :: Int -> Type a
  var = Var . Name

occurs :: (Occurrences a) => Name -> Env -> Type a -> Bool
occurs = (((> 0) .) .) . occurrences

class Occurrences a where
  occurrences :: Name -> Env -> Type a -> Int

instance Occurrences Row where
  occurrences name env type_ = case type_ of
    a :. b -> occurrences name env a + occurrences name env b
    Empty -> 0
    Test -> 0
    Var name' -> case retrieve env (row name') of
      Left{} -> if name == name' then 1 else 0
      Right type' -> occurrences name env type'

instance Occurrences Scalar where
  occurrences name env type_ = case type_ of
    a :& b -> occurrences name env a + occurrences name env b
    (:?) a -> occurrences name env a
    a :| b -> occurrences name env a + occurrences name env b
    Function a b _
      -> occurrences name env a
      + occurrences name env b
    Bool -> 0
    Char -> 0
    Float -> 0
    Handle -> 0
    Int -> 0
    Test -> 0
    Var name' -> case retrieve env (scalar name') of
      Left{} -> if name == name' then 1 else 0
      Right type' -> occurrences name env type'
    Unit -> 0
    Vector a -> occurrences name env a

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween
  :: forall a. Occurrences a
  => Env
  -> Env
  -> TypeName a
  -> Bool
dependentBetween before after name
  = any (bound after) (unbound before)
  where
  bound env name'
    = occurs (typeName name) env (Var name' :: Type a)

-- | Enumerates those type variables in an environment which
-- are allocated but not yet bound to a type.
unbound :: Env -> [Name]
unbound Env{..} = filter
  (not . (`Map.member` envScalars))
  [Name 0 .. pred envNext]

class Simplify a where
  simplify :: Env -> Type a -> Type a

instance Simplify Row where
  simplify env type_ = case type_ of
    Var name
      | Right type' <- retrieve env (row name)
      -> simplify env type'
    _ -> type_

instance Simplify Scalar where
  simplify env type_ = case type_ of
    Var name
      | Right type' <- retrieve env (scalar name)
      -> simplify env type'
    _ -> type_

class Substitute a where
  sub :: Env -> Type a -> Type a

instance Substitute Row where
  sub env type_ = case type_ of
    a :. b -> sub env a :. sub env b
    Empty{} -> type_
    Test{} -> type_
    Var name
      | Right type' <- retrieve env (row name)
      -> sub env type'
      | otherwise
      -> type_

instance Substitute Scalar where
  sub env type_ = case type_ of
    Function a b p -> Function (sub env a) (sub env b) p
    a :& b -> sub env a :& sub env b
    (:?) a -> (sub env a :?)
    a :| b -> sub env a :| sub env b
    Bool{} -> type_
    Char{} -> type_
    Float{} -> type_
    Int{} -> type_
    Handle{} -> type_
    Test -> type_
    Var name
      | Right type' <- retrieve env (scalar name)
      -> sub env type'
      | otherwise
      -> type_
    Unit{} -> type_
    Vector a -> Vector (sub env a)
