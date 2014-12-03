{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer
import Data.Foldable (foldrM)
import Data.List
import Data.Monoid
import Data.Set (Set)

import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Vector as V

import Kitten.Id
import Kitten.Infer.Monad
import Kitten.Location
import Kitten.Kind
import Kitten.KindedId
import Kitten.Program
import Kitten.Type
import Kitten.Util.Function
import Kitten.Util.Monad

import qualified Kitten.IdMap as Id

instantiateM :: TypeScheme -> K (Type Scalar)
instantiateM scheme = do
  loc <- getsProgram inferenceLocation
  liftState $ state (instantiate loc scheme)

instantiate
  :: Location
  -> TypeScheme
  -> Program
  -> (Type Scalar, Program)
instantiate loc (Forall stacks scalars type_) program
  = (sub renamed type_, program')

  where
  (renamed, program') = flip runState program
    $ composeM [renames stacks, renames scalars] emptyProgram

  renames
    :: (Declare a, Fresh a)
    => Set (KindedId a)
    -> Program
    -> State Program Program
  renames = flip (foldrM rename) . S.toList

  rename
    :: (Declare a, Fresh a)
    => KindedId a
    -> Program
    -> State Program Program
  rename name local = do
    var <- state (freshVar loc)
    return (declare name var local)

generalize :: K (a, Type Scalar) -> K (a, TypeScheme)
generalize action = do
  before <- getProgram
  (extra, type_) <- action  -- HACK To preserve effect ordering.
  after <- getProgram

  let
    substituted :: Type Scalar
    substituted = sub after type_

    dependent :: (Occurrences a, ReifyKind a, Unbound a) => KindedId a -> Bool
    dependent = dependentBetween before after

    scalars :: [KindedId Scalar]
    stacks :: [KindedId Stack]
    (stacks, scalars) = (filter dependent *** filter dependent)
      (freeVars substituted)

  return . (,) extra . regeneralize after $ Forall
    (S.fromList stacks)
    (S.fromList scalars)
    substituted

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween
  :: forall a. (Occurrences a, ReifyKind a, Unbound a)
  => Program
  -> Program
  -> KindedId a
  -> Bool
dependentBetween before after name
  = any (bound after) (unbound before)
  where
  bound :: Program -> KindedId a -> Bool
  bound env name' = occurs (unkinded name) env
    (TyVar name' (inferenceLocation env) :: Type a)

-- | Enumerates those type variables in an environment that
-- are allocated but not yet bound to a type.
class Unbound (a :: Kind) where
  unbound :: Program -> [KindedId a]

instance Unbound Scalar where
  unbound env = map KindedId
    $ filter (`Id.notMember` inferenceScalars env)
    $ let n = envMaxScalar env in [n, pred n .. Id 0]
    -- See note [enumerating unbound names].

instance Unbound Stack where
  unbound env = map KindedId
    $ filter (`Id.notMember` inferenceStacks env)
    $ let n = envMaxStack env in [n, pred n .. Id 0]
    -- See note [enumerating unbound names].

-- | The last allocated name in an environment. Relies on
-- the fact that 'NameGen' allocates names sequentially.
envMaxScalar :: Program -> Id TypeSpace
envMaxScalar = unkinded . pred . fst . genKinded . programScalarIdGen

-- | See 'envMaxScalar'.
envMaxStack :: Program -> Id TypeSpace
envMaxStack = unkinded . pred . fst . genKinded . programStackIdGen

-- Note [enumerating unbound names]:
--
-- We enumerate unbound names in descending order, with the most recently
-- allocated names appearing first, so that searches for bound names complete
-- faster on average and do fewer allocations.

data TypeLevel = TopLevel | NonTopLevel
  deriving (Eq)

regeneralize :: Program -> TypeScheme -> TypeScheme
regeneralize program (Forall stacks scalars wholeType) = let
  (type_, vars) = runWriter $ regeneralize' TopLevel wholeType
  in Forall (foldr S.delete stacks vars) scalars type_
  where
  regeneralize'
    :: forall a. TypeLevel -> Type a -> Writer [KindedId Stack] (Type a)
  regeneralize' level type_ = case type_ of
    TyFunction a b loc
      | level == NonTopLevel
      , TyVar c _ <- bottommost a
      , TyVar d _ <- bottommost b
      , c == d
      -> do
        -- If this is the only mention of this type variable, then it
        -- can simply be removed from the outer quantifier. Otherwise,
        -- it should be renamed in the inner quantifier.
        when (occurrences
          (reifyKind (KindProxy :: KindProxy a))
          (unkinded c) program wholeType == 2)
          $ tell [c]
        return $ TyQuantified
          (Forall (S.singleton c) S.empty type_)
          loc
    TyFunction a b loc -> TyFunction
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
  -> ([KindedId Stack], [KindedId Scalar])
freeVars type_
  = let (stacks, scalars) = free type_
  in (nub stacks, nub scalars)

class Free a where
  free :: a -> ([KindedId Stack], [KindedId Scalar])

instance Free (Type Stack) where
  free type_ = case type_ of
    a :. b -> free a <> free b
    TyConst name _ -> ([name], [])
    TyEmpty{} -> mempty
    TyVar name _ -> ([name], [])

instance Free (Type Scalar) where
  free type_ = case type_ of
    a :& b -> free a <> free b
    (:?) a -> free a
    a :@ bs -> free a <> F.foldMap free bs
    a :| b -> free a <> free b
    TyFunction a b _ -> free a <> free b
    TyConst name _ -> ([], [name])
    TyCtor{} -> mempty
    TyQuantified (Forall r s t) _
      -> let (stacks, scalars) = free t
      in (stacks \\ S.toList r, scalars \\ S.toList s)
    TyVar name _ -> ([], [name])
    TyVector a _ -> free a

instance Free TypeScheme where
  free (Forall stacks scalars type_) = let
    (stacks', scalars') = free type_
    in (stacks' \\ S.toList stacks, scalars' \\ S.toList scalars)

normalize :: Location -> Type Scalar -> Type Scalar
normalize loc type_ = let
  (stacks, scalars) = freeVars type_
  stackCount = length stacks
  program = emptyProgram
    { inferenceStacks = Id.fromList
      $ zip (map unkinded stacks) (map var [0..])
    , inferenceScalars = Id.fromList
      $ zip (map unkinded scalars) (map var [stackCount..])
    }
  in sub program type_
  where
  var :: Int -> Type a
  var index = TyVar (KindedId (Id index)) loc

occurs
  :: forall a
  . (Occurrences a, ReifyKind a)
  => Id TypeSpace -> Program -> Type a -> Bool
occurs = (> 0) ..: occurrences (reifyKind (KindProxy :: KindProxy a))

class Occurrences a where
  occurrences :: Kind -> Id TypeSpace -> Program -> Type a -> Int

instance Occurrences Stack where
  occurrences kind name program type_ = case type_ of
    a :. b -> recur a + recur b
    TyConst typeName@(KindedId name') _ -> case retrieve program typeName of
      Left{} -> if kind == Stack && name == name' then 1 else 0
      Right type' -> recur type'
    TyEmpty{} -> 0
    TyVar typeName@(KindedId name') _ -> case retrieve program typeName of
      Left{} -> if kind == Stack && name == name' then 1 else 0
      Right type' -> recur type'
    where
    recur :: (Occurrences a) => Type a -> Int
    recur = occurrences kind name program

instance Occurrences Scalar where
  occurrences kind name program type_ = case type_ of
    a :& b -> recur a + recur b
    (:?) a -> recur a
    a :@ bs -> recur a + V.sum (V.map recur bs)
    a :| b -> recur a + recur b
    TyFunction a b _ -> recur a + recur b
    TyConst typeName@(KindedId name') _ -> case retrieve program typeName of
      Left{} -> if kind == Scalar && name == name' then 1 else 0
      Right type' -> recur type'
    TyCtor{} -> 0
    TyVar typeName@(KindedId name') _ -> case retrieve program typeName of
      Left{} -> if kind == Scalar && name == name' then 1 else 0
      Right type' -> recur type'
    TyQuantified (Forall _ s t) _
      -> if KindedId name `S.member` s then 0 else recur t
    TyVector a _ -> recur a
    where
    recur :: (Occurrences a) => Type a -> Int
    recur = occurrences kind name program

class Simplify a where
  simplify :: Program -> Type a -> Type a

instance Simplify Stack where
  simplify program type_ = case type_ of
    TyVar name _
      | Right type' <- retrieve program name
      -> simplify program type'
    _ -> type_

instance Simplify Scalar where
  simplify program type_ = case type_ of
    TyVar name _
      | Right type' <- retrieve program name
      -> simplify program type'
    _ -> type_

class Substitute a where
  sub :: Program -> Type a -> Type a

instance Substitute Stack where
  sub program type_ = case type_ of
    a :. b -> sub program a :. sub program b
    TyConst{} -> type_  -- See Note [Constant Substitution].
    TyEmpty{} -> type_
    TyVar name _
      | Right type' <- retrieve program name
      -> sub program type'
      | otherwise
      -> type_

instance Substitute Scalar where
  sub program type_ = case type_ of
    TyFunction a b origin -> TyFunction (recur a) (recur b) origin
    a :& b -> recur a :& recur b
    (:?) a -> (recur a :?)
    a :@ bs -> recur a :@ V.map recur bs
    a :| b -> recur a :| recur b
    TyConst{} -> type_  -- See Note [Constant Substitution].
    TyCtor{} -> type_
    TyQuantified (Forall r s t) loc
      -> TyQuantified (Forall r s (recur t)) loc
    TyVar name _
      | Right type' <- retrieve program name
      -> recur type'
      | otherwise
      -> type_
    TyVector a loc -> TyVector (recur a) loc
    where
    recur :: (Substitute a) => Type a -> Type a
    recur = sub program

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
  -> K ([KindedId Stack], [KindedId Scalar], Type Scalar)
skolemize (Forall stackVars scalarVars type_) = do
  loc <- getsProgram inferenceLocation
  let
    declares
      :: (Declare a)
      => Set (KindedId a) -> [KindedId a] -> Program -> Program
    declares vars consts program0
      = foldr (uncurry declare) program0
      $ zip (S.toList vars) (map (\name -> TyConst name loc) consts)
  scalarConsts <- replicateM (S.size scalarVars) freshScalarIdM
  stackConsts <- replicateM (S.size stackVars) freshStackIdM
  program <- getsProgram
    $ declares scalarVars scalarConsts
    . declares stackVars stackConsts
  (stackConsts', scalarConsts', type') <- skolemizeType (sub program type_)
  return (stackConsts ++ stackConsts', scalarConsts ++ scalarConsts', type')

skolemizeType
  :: Type a
  -> K ([KindedId Stack], [KindedId Scalar], Type a)
skolemizeType = \case
  TyFunction a b loc -> do
    (stackConsts, scalarConsts, b') <- skolemizeType b
    return (stackConsts, scalarConsts, TyFunction a b' loc)
  TyQuantified scheme _ -> skolemize scheme
  type_ -> return ([], [], type_)
