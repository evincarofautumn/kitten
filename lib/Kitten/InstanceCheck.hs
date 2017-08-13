{-|
Module      : Kitten.InstanceCheck
Description : Checking types against signatures
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.InstanceCheck
  ( instanceCheck
  ) where

import Control.Monad (forM_, unless)
import Data.List (find)
import Data.Set (Set)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K, attempt)
import Kitten.Origin (Origin, getOrigin)
import Kitten.Type (Constructor(..), Type(..), TypeId, Var(..))
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Kitten.Free as Free
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Report as Report
import qualified Kitten.Substitute as Substitute
import qualified Kitten.Type as Type
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Unify as Unify
import qualified Kitten.Zonk as Zonk
import qualified Text.PrettyPrint as Pretty

-- | Checks whether one type is a generic instance of another, used for checking
-- type signatures. Remember, when using this function, which way the subtyping
-- relation goes: @∀α. α → α@ is a generic instance of @int → int@, not the
-- other way around!

instanceCheck :: Pretty.Doc -> Type -> Pretty.Doc -> Type -> K ()
instanceCheck aSort aScheme bSort bScheme = withContext $ do
  let tenv0 = TypeEnv.empty
  let aType = aScheme
  (ids, bType) <- skolemize tenv0 bScheme
  let envTypes = Map.elems (TypeEnv.tvs tenv0)
  success <- attempt $ subsumptionCheck tenv0 aType bType
  unless success failure
  let escaped = Set.unions $ map (Free.tvs tenv0) (aScheme : bScheme : envTypes)
  -- Free.tvs tenv0 aScheme `Set.union` Free.tvs tenv0 bScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) failure
  return ()
  where
  failure = report $ Report.FailedInstanceCheck aScheme bScheme
  withContext
    = while (getOrigin aScheme)
      (Pretty.hsep
        [ "checking"
        , aSort
        , "type"
        , Pretty.quotes (pPrint aScheme)
        ])
    . while (getOrigin bScheme)
      (Pretty.hsep
        [ "against"
        , bSort
        , "type"
        , Pretty.quotes (pPrint bScheme)
        ])

-- | Skolemization replaces each quantified type variable with a type constant
-- that unifies only with itself.

skolemize :: TypeEnv -> Type -> K (Set TypeId, Type)
skolemize tenv0 t = case t of
  Forall origin (Var name x k) t' -> do
    c <- freshTypeId tenv0
    substituted <- Substitute.type_ tenv0 x
      (TypeConstant origin $ Var name c k) t'
    (c', t'') <- skolemize tenv0 substituted
    return (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  TypeConstructor origin "Fun" :@ a :@ b :@ e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, Type.fun origin a b' e)
  _ -> return (Set.empty, t)

-- | Subsumption checking is largely the same as unification, accounting for
-- function type variance: if @(a -> b) <: (c -> d)@ then @b <: d@ (covariant)
-- but @c <: a@ (contravariant).

subsumptionCheck :: TypeEnv -> Type -> Type -> K TypeEnv
subsumptionCheck tenv0 (Forall origin (Var name x k) t) t2 = do
  (t1, _, tenv1) <- Instantiate.type_ tenv0 origin name x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 (TypeConstructor _ "Fun" :@ a' :@ b' :@ e') = do
  (a, b, e, tenv1) <- Unify.function tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 (TypeConstructor _ "Fun" :@ a :@ b :@ e) t2 = do
  (a', b', e', tenv1) <- Unify.function tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 t1 t2 = Unify.type_ tenv0 t1 t2

subsumptionCheckFun
  :: TypeEnv -> Type -> Type -> Type -> Type -> Type -> Type -> K TypeEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  let
    labels = permissionList $ Zonk.type_ tenv2 e
    labels' = permissionList $ Zonk.type_ tenv2 e'
  forM_ labels $ \ (origin, label) -> case find ((label ==) . snd) labels' of
    Just{} -> return ()
    Nothing -> report $ Report.MissingPermissionLabel e e' origin label
  return tenv2
  where

  permissionList :: Type -> [(Origin, Constructor)]
  permissionList (TypeConstructor _ "Join" :@ TypeConstructor origin label :@ es)
    = (origin, label) : permissionList es
  permissionList _ = []
