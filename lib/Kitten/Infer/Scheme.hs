{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PostfixOperators #-}
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
  , skolemize
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Foldable (foldrM)
import Data.List
import Data.Monoid
import Data.Set (Set)

import qualified Data.Set as S

import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type
import Kitten.Util.Monad

import qualified Kitten.NameMap as N

instantiateM :: TypeScheme -> Inferred (Type Scalar)
instantiateM scheme = do
  origin <- getsEnv envOrigin
  liftState $ state (instantiate origin scheme)

instantiate :: Origin -> TypeScheme -> Env -> (Type Scalar, Env)
instantiate origin (Forall rows scalars type_) env
  = (sub renamed type_, env')

  where
  renamed :: Env
  env' :: Env
  (renamed, env') = flip runState env $ composeM
    [renames rows, renames scalars] emptyEnv

  renames
    :: (Declare a)
    => Set (TypeName a)
    -> Env
    -> State Env Env
  renames = flip (foldrM rename) . S.toList

  rename
    :: (Declare a)
    => TypeName a
    -> Env
    -> State Env Env
  rename name localEnv = do
    var <- state (freshVar origin)
    return (declare name var localEnv)

generalize :: Type Scalar -> Inferred TypeScheme
generalize type_ = do
  after <- getEnv

  let
    substituted :: Type Scalar
    substituted = sub after type_

    rows :: [TypeName Row]
    scalars :: [TypeName Scalar]
    (rows, scalars) = freeVars substituted

  return $ Forall
    (S.fromList rows)
    (S.fromList scalars)
    substituted

freeVars
  :: (Free (Type a))
  => Type a
  -> ([TypeName Row], [TypeName Scalar])
freeVars type_
  = let (rows, scalars) = free type_
  in (nub rows, nub scalars)

class Free a where
  free :: a -> ([TypeName Row], [TypeName Scalar])

instance Free (Type Row) where
  free type_ = case type_ of
    a :. b -> free a <> free b
    Empty{} -> mempty
    Var name _ -> ([name], [])

instance Free (Type Scalar) where
  free type_ = case type_ of
    a :& b -> free a <> free b
    (:?) a -> free a
    a :| b -> free a <> free b
    Function a b _ -> free a <> free b
    Bool{} -> mempty
    Char{} -> mempty
    Const name _ -> ([], [name])
    Float{} -> mempty
    Handle{} -> mempty
    Int{} -> mempty
    Named{} -> mempty
    Var name _ -> ([], [name])
    Unit{} -> mempty
    Vector a _ -> free a

instance Free TypeScheme where
  free (Forall rows scalars type_) = let
    (rows', scalars') = free type_
    in (rows' \\ S.toList rows, scalars' \\ S.toList scalars)

normalize :: Origin -> Type Scalar -> Type Scalar
normalize origin type_ = let
  (rows, scalars) = freeVars type_
  rowCount = length rows
  env = emptyEnv
    { envRows = N.fromList
      $ zip (map unTypeName rows) (map var [0..])
    , envScalars = N.fromList
      $ zip (map unTypeName scalars) (map var [rowCount..])
    }
  in sub env type_
  where
  var :: Int -> Type a
  var index = Var (TypeName (Name index)) origin

occurs :: (Occurrences a) => Name -> Env -> Type a -> Bool
occurs = (((> 0) .) .) . occurrences

class Occurrences a where
  occurrences :: Name -> Env -> Type a -> Int

instance Occurrences Row where
  occurrences name env type_ = case type_ of
    a :. b -> occurrences name env a + occurrences name env b
    Empty{} -> 0
    Var typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'

instance Occurrences Scalar where
  occurrences name env type_ = case type_ of
    a :& b -> occurrences name env a + occurrences name env b
    (:?) a -> occurrences name env a
    a :| b -> occurrences name env a + occurrences name env b
    Function a b _ -> occurrences name env a + occurrences name env b
    Bool{} -> 0
    Char{} -> 0
    Const typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
    Float{} -> 0
    Handle{} -> 0
    Int{} -> 0
    Named{} -> 0
    Var typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
    Unit{} -> 0
    Vector a _ -> occurrences name env a

-- Note [Var Kinds]:
--
-- Type variables are allocated such that, if the 'Kind's of
-- two type variables are not equal, the 'Names' of those
-- two type variables are also not equal. Additionally,
-- skolem constants ('Const') do not overlap with type
-- variables ('Var').

class Simplify a where
  simplify :: Env -> Type a -> Type a

instance Simplify Row where
  simplify env type_ = case type_ of
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> simplify env type' `addHint` hint
    _ -> type_

instance Simplify Scalar where
  simplify env type_ = case type_ of
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> simplify env type' `addHint` hint
    _ -> type_

class Substitute a where
  sub :: Env -> Type a -> Type a

instance Substitute Row where
  sub env type_ = case type_ of
    a :. b -> sub env a :. sub env b
    Empty{} -> type_
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> sub env type' `addHint` hint
      | otherwise
      -> type_

instance Substitute Scalar where
  sub env type_ = case type_ of
    Function a b origin -> Function
      (sub env a)
      (sub env b)
      origin
    a :& b -> sub env a :& sub env b
    (:?) a -> (sub env a :?)
    a :| b -> sub env a :| sub env b
    Bool{} -> type_
    Char{} -> type_
    Const{} -> type_  -- See Note [Constant Substitution].
    Float{} -> type_
    Int{} -> type_
    Handle{} -> type_
    Named{} -> type_
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> sub env type' `addHint` hint
      | otherwise
      -> type_
    Unit{} -> type_
    Vector a origin -> Vector (sub env a) origin

-- Note [Constant Substitution]:
--
-- Skolem constants do not unify with anything but
-- themselves, so they never appear as substitutions in the
-- typing environment. Therefore, it is unnecessary to
-- substitute on them.

skolemize
  :: TypeScheme
  -> Inferred ([TypeName Scalar], Type Scalar)
skolemize (Forall _rowVars scalarVars type_) = do
  origin <- getsEnv envOrigin
  let
    declares :: Set (TypeName Scalar) -> [TypeName Scalar] -> Env -> Env
    declares vars consts env0
      = foldr (uncurry declare) env0
      $ zip (S.toList vars) (map (\name -> Const name origin) consts)
  consts <- replicateM (S.size scalarVars) freshNameM
  env <- getsEnv $ declares scalarVars consts
  (consts', type') <- skolemizeType (sub env type_)
  return (consts ++ consts', type')

-- This is currently unnecessary, but will be required when
-- quantifiers are allowed to be non-prenex.
skolemizeType
  :: Type a
  -> Inferred ([TypeName Scalar], Type a)
skolemizeType = \case
  Function a b origin -> do
    (consts, b') <- skolemizeType b
    return (consts, Function a b' origin)
  type_ -> return ([], type_)
