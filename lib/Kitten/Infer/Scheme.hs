{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
  , skolemize
  ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer
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
instantiate origin@(Origin _ loc) (Forall stacks scalars type_) env
  = (sub renamed type_, env')

  where
  renamed :: Env
  env' :: Env
  (renamed, env') = flip runState env $ composeM
    [renames stacks, renames scalars] (emptyEnv loc)

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

generalize :: Inferred (a, Type Scalar) -> Inferred (a, TypeScheme)
generalize action = do
  before <- getEnv
  (extra, type_) <- action  -- HACK To preserve effect ordering.
  after <- getEnv

  let
    substituted :: Type Scalar
    substituted = sub after type_

    dependent :: (Occurrences a, Unbound a) => TypeName a -> Bool
    dependent = dependentBetween before after

    scalars :: [TypeName Scalar]
    stacks :: [TypeName Stack]
    (stacks, scalars) = (filter dependent *** filter dependent)
      (freeVars substituted)

  return . (,) extra . regeneralize after $ Forall
    (S.fromList stacks)
    (S.fromList scalars)
    substituted

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween
  :: forall a. (Occurrences a, Unbound a)
  => Env
  -> Env
  -> TypeName a
  -> Bool
dependentBetween before after name
  = any (bound after) (unbound before)
  where
  bound :: Env -> TypeName a -> Bool
  bound env name' = occurs (unTypeName name) env
    (Var name' (envOrigin env) :: Type a)

-- | Enumerates those type variables in an environment that
-- are allocated but not yet bound to a type.
class Unbound (a :: Kind) where
  unbound :: Env -> [TypeName a]

instance Unbound Scalar where
  unbound env = map TypeName $ filter (`N.notMember` envScalars env)
    [Name 0 .. envMaxName env]

instance Unbound Stack where
  unbound env = map TypeName $ filter (`N.notMember` envStacks env)
    [Name 0 .. envMaxName env]

-- | The last allocated name in an environment. Relies on
-- the fact that 'NameGen' allocates names sequentially.
envMaxName :: Env -> Name
envMaxName = pred . fst . genName . envNameGen

data TypeLevel = TopLevel | NonTopLevel
  deriving (Eq)

regeneralize :: Env -> TypeScheme -> TypeScheme
regeneralize env (Forall stacks scalars wholeType) = let
  (type_, vars) = runWriter $ regeneralize' TopLevel wholeType
  in Forall (foldr S.delete stacks vars) scalars type_
  where
  regeneralize' :: TypeLevel -> Type a -> Writer [TypeName Stack] (Type a)
  regeneralize' level type_ = case type_ of
    Function a b loc
      | level == NonTopLevel
      , Var c _ <- bottommost a
      , Var d _ <- bottommost b
      , c == d
      -> do
        -- If this is the only mention of this type variable, then it
        -- can simply be removed from the outer quantifier. Otherwise,
        -- it should be renamed in the inner quantifier.
        when (occurrences (unTypeName c) env wholeType == 2)
          $ tell [c]
        return $ Quantified
          (Forall (S.singleton c) S.empty type_)
          loc
    Function a b loc -> Function
      <$> regeneralize' NonTopLevel a
      <*> regeneralize' NonTopLevel b
      <*> pure loc
    a :. b -> (:.)
      <$> regeneralize' NonTopLevel a
      <*> regeneralize' NonTopLevel b
    (:?) a -> (:?)
      <$> regeneralize' NonTopLevel a
    a :| b -> (:|)
      <$> regeneralize' NonTopLevel a
      <*> regeneralize' NonTopLevel b
    _ -> return type_

freeVars
  :: (Free (Type a))
  => Type a
  -> ([TypeName Stack], [TypeName Scalar])
freeVars type_
  = let (stacks, scalars) = free type_
  in (nub stacks, nub scalars)

class Free a where
  free :: a -> ([TypeName Stack], [TypeName Scalar])

instance Free (Type Stack) where
  free type_ = case type_ of
    a :. b -> free a <> free b
    Const name _ -> ([name], [])
    Empty{} -> mempty
    Var name _ -> ([name], [])

instance Free (Type Scalar) where
  free type_ = case type_ of
    a :& b -> free a <> free b
    (:?) a -> free a
    a :| b -> free a <> free b
    Function a b _ -> free a <> free b
    Const name _ -> ([], [name])
    Ctor{} -> mempty
    Quantified (Forall r s t) _
      -> let (stacks, scalars) = free t
      in (stacks \\ S.toList r, scalars \\ S.toList s)
    Var name _ -> ([], [name])
    Vector a _ -> free a

instance Free TypeScheme where
  free (Forall stacks scalars type_) = let
    (stacks', scalars') = free type_
    in (stacks' \\ S.toList stacks, scalars' \\ S.toList scalars)

normalize :: Origin -> Type Scalar -> Type Scalar
normalize origin@(Origin _ loc) type_ = let
  (stacks, scalars) = freeVars type_
  stackCount = length stacks
  env = (emptyEnv loc)
    { envStacks = N.fromList
      $ zip (map unTypeName stacks) (map var [0..])
    , envScalars = N.fromList
      $ zip (map unTypeName scalars) (map var [stackCount..])
    }
  in sub env type_
  where
  var :: Int -> Type a
  var index = Var (TypeName (Name index)) origin

occurs :: (Occurrences a) => Name -> Env -> Type a -> Bool
occurs = (((> 0) .) .) . occurrences

class Occurrences a where
  occurrences :: Name -> Env -> Type a -> Int

instance Occurrences Stack where
  occurrences name env type_ = case type_ of
    a :. b -> occurrences name env a + occurrences name env b
    Const typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
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
    Const typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
    Ctor{} -> 0
    Var typeName@(TypeName name') _ -> case retrieve env typeName of
      Left{} -> if name == name' then 1 else 0  -- See Note [Var Kinds].
      Right type' -> occurrences name env type'
    Quantified (Forall _ s t) _
      -> if TypeName name `S.member` s then 0 else occurrences name env t
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

instance Simplify Stack where
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

instance Substitute Stack where
  sub env type_ = case type_ of
    a :. b -> sub env a :. sub env b
    Const{} -> type_  -- See Note [Constant Substitution].
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
    Const{} -> type_  -- See Note [Constant Substitution].
    Ctor{} -> type_
    Quantified (Forall r s t) loc
      -> Quantified (Forall r s (sub env t)) loc
    Var name (Origin hint _loc)
      | Right type' <- retrieve env name
      -> sub env type' `addHint` hint
      | otherwise
      -> type_
    Vector a origin -> Vector (sub env a) origin

-- Note [Constant Substitution]:
--
-- Skolem constants do not unify with anything but
-- themselves, so they never appear as substitutions in the
-- typing environment. Therefore, it is unnecessary to
-- substitute on them.

-- | Skolemizes a type scheme by replacing all of the bound
-- variables with skolem constants. Returns the skolem
-- constants and the skolemized type.
skolemize
  :: TypeScheme
  -> Inferred ([TypeName Stack], [TypeName Scalar], Type Scalar)
skolemize (Forall stackVars scalarVars type_) = do
  origin <- getsEnv envOrigin
  let
    declares
      :: (Declare a)
      => Set (TypeName a) -> [TypeName a] -> Env -> Env
    declares vars consts env0
      = foldr (uncurry declare) env0
      $ zip (S.toList vars) (map (\name -> Const name origin) consts)
  stackConsts <- replicateM (S.size stackVars) freshNameM
  scalarConsts <- replicateM (S.size scalarVars) freshNameM
  env <- getsEnv
    $ declares scalarVars scalarConsts
    . declares stackVars stackConsts
  (stackConsts', scalarConsts', type') <- skolemizeType (sub env type_)
  return (stackConsts ++ stackConsts', scalarConsts ++ scalarConsts', type')

skolemizeType
  :: Type a
  -> Inferred ([TypeName Stack], [TypeName Scalar], Type a)
skolemizeType = \case
  Function a b origin -> do
    (stackConsts, scalarConsts, b') <- skolemizeType b
    return (stackConsts, scalarConsts, Function a b' origin)
  Quantified scheme _ -> skolemize scheme
  type_ -> return ([], [], type_)
