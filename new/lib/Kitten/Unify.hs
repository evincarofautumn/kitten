{-# LANGUAGE OverloadedStrings #-}

module Kitten.Unify
  ( function
  , type_
  ) where

import Kitten.Informer (Informer(..))
import Kitten.Kind (Kind(..))
import Kitten.Monad (K)
import Kitten.Occurrences (occurs)
import Kitten.Origin (Origin)
import Kitten.Type (Type(..), TypeId, Var(..), funType, joinType)
import Kitten.TypeEnv (TypeEnv, freshTv)
import qualified Data.Map as Map
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Report as Report
import qualified Kitten.Type as Type
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Zonk as Zonk

-- There are two kinds of unification going on here: basic logical unification
-- for value types, and row unification for permission types.

type_ :: TypeEnv -> Type -> Type -> K TypeEnv
type_ tenv0 t1 t2 = case (t1', t2') of
  _ | t1' == t2' -> return tenv0
  (TypeVar origin x, t) -> unifyTv tenv0 origin x t
  (_, TypeVar{}) -> commute
  -- FIXME: Unify the kinds here?
  (a, Forall origin (Var x k) t) -> do
    (b, _, tenv1) <- Instantiate.type_ tenv0 origin x k t
    type_ tenv1 a b
  (Forall{}, _) -> commute

  (TypeConstructor _ "Join" :@ l :@ r, s) -> do
    ms <- rowIso tenv0 l s (permissionTail r)
    case ms of
      Just (s', substitution, tenv1)
        -> case substitution of
          Just (x, t) -> let
            tenv2 = tenv1 { TypeEnv.tvs = Map.insert x t $ TypeEnv.tvs tenv1 }
            in type_ tenv2 r s'
          Nothing -> type_ tenv1 r s'

      Nothing -> do
        report $ Report.TypeMismatch t1' t2'
        halt

  (_, TypeConstructor _ "Join" :@ _ :@ _) -> commute

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and permission type constructors.

  (a :@ b, c :@ d) -> do
    tenv1 <- type_ tenv0 a c
    type_ tenv1 b d

  _ -> do
    report $ Report.TypeMismatch t1' t2'
    halt

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  t1' = Zonk.type_ tenv0 t1
  t2' = Zonk.type_ tenv0 t2
  commute = type_ tenv0 t2 t1
  permissionTail (TypeConstructor _ "Join" :@ _ :@ a) = permissionTail a
  permissionTail t = t

-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.
--
-- See: Occurs Checks

unifyTv :: TypeEnv -> Origin -> Var -> Type -> K TypeEnv
unifyTv tenv0 origin v@(Var x _) t = case t of
  TypeVar _origin (Var y _) | x == y -> return tenv0
  TypeVar{} -> declare
  _ -> if occurs tenv0 x t
    then let t' = Zonk.type_ tenv0 t in do
      report $ Report.Chain $
        [ Report.TypeMismatch (TypeVar origin v) t'
        , Report.OccursCheckFailure (TypeVar origin v) t'
        ] ++ case t' of
          TypeConstructor _ "Prod" :@ _ :@ _ -> [Report.StackDepthMismatch (Type.origin t')]
          _ -> []
      halt
    else declare
  where
  declare = return tenv0 { TypeEnv.tvs = Map.insert x t $ TypeEnv.tvs tenv0 }

-- A convenience function for unifying a type with a function type.

function :: TypeEnv -> Type -> K (Type, Type, Type, TypeEnv)
function tenv0 t = case t of
  TypeConstructor _ "Fun" :@ a :@ b :@ e -> return (a, b, e, tenv0)
  _ -> do
    let origin = Type.origin t
    a <- freshTv tenv0 origin Stack
    b <- freshTv tenv0 origin Stack
    e <- freshTv tenv0 origin Permission
    tenv1 <- type_ tenv0 t $ funType origin a b e
    return (a, b, e, tenv1)

-- Row unification is essentially unification of sets. The row-isomorphism
-- operation (as described in [1]) takes a permission label and a permission
-- row, and asserts that the row can be rewritten to begin with that label under
-- some substitution. It returns the substitution and the tail of the rewritten
-- row. The substitution is always either empty (∅) or a singleton substitution
-- (x ↦ τ), so we represent this as a 'Maybe'.

rowIso
  :: TypeEnv -> Type -> Type -> Type
  -> K (Maybe (Type, Maybe (TypeId, Type), TypeEnv))

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 l (TypeConstructor _ "Join" :@ l' :@ r') _
  | l == l' = return $ Just (r', Nothing :: Maybe (TypeId, Type), tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l (TypeConstructor origin "Join" :@ l' :@ r') rt
  | l /= l' = do
  ms <- rowIso tenv0 l r' rt
  return $ case ms of
    Just (r'', substitution, tenv1) -> Just
      (joinType origin l' r'', substitution, tenv1)
    Nothing -> Nothing

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail. Here we enforce a side condition
-- that ensures termination by preventing unification of rows with a common tail
-- but distinct prefixes.

rowIso tenv0 l r@(TypeVar origin (Var a _)) rt
  | r /= rt = do
  b <- freshTv tenv0 origin Permission
  return $ Just (b, Just (a, joinType origin l b), tenv0)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ _ = return Nothing
