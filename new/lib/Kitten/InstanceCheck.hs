{-# LANGUAGE OverloadedStrings #-}

module Kitten.InstanceCheck
  ( instanceCheck
  ) where

import Control.Monad (forM_, unless)
import Data.List (find)
import Data.Set (Set)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K, attempt)
import Kitten.Origin (Origin)
import Kitten.Report (Category(..), Item(..), Report(..))
import Kitten.Type (Constructor(..), Type(..), TypeId, Var(..), funType)
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Kitten.Free as Free
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Type as Type
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Unify as Unify
import qualified Kitten.Zonk as Zonk
import qualified Text.PrettyPrint as Pretty

-- Since skolem constants only unify with type variables and themselves,
-- unifying a skolemized scheme with a type tells you whether one is a generic
-- instance of the other. This is used to check the signatures of definitions.

instanceCheck :: Pretty.Doc -> Type -> Pretty.Doc -> Type -> K ()
instanceCheck aSort aScheme bSort bScheme = do
  let tenv0 = TypeEnv.empty
  (aType, _, tenv1) <- Instantiate.prenex tenv0 aScheme
  (ids, bType) <- skolemize tenv1 bScheme
  success <- attempt $ subsumptionCheck tenv1 aType bType
  unless success failure
  let escaped = Free.tvs aScheme `Set.union` Free.tvs bScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) failure
  return ()
  where
  failure = report $ Report Error
    [ Item (Type.origin aScheme)
      ["I can't match this", aSort, "type", Pretty.quote aScheme]
    , Item (Type.origin bScheme)
      [ "with the", bSort, "type signature"
      , Pretty.quote bScheme
      ]
    ]

-- Skolemization replaces quantified type variables with type constants.

skolemize :: TypeEnv -> Type -> K (Set TypeId, Type)
skolemize tenv0 t = case t of
  Forall origin (Var x k) t' -> do
    c <- freshTypeId tenv0
    let tenv1 = tenv0 { TypeEnv.tvs = Map.insert x (TypeConstant origin $ Var c k)
        $ TypeEnv.tvs tenv0 }
    (c', t'') <- skolemize tenv1 $ Zonk.type_ tenv1 t'
    return (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  TypeConstructor origin "fun" :@ a :@ b :@ e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, funType origin a b' e)
  _ -> return (Set.empty, t)

-- Subsumption checking is largely the same as unification, except for the fact
-- that a function type is contravariant in its input type.

subsumptionCheck :: TypeEnv -> Type -> Type -> K TypeEnv
subsumptionCheck tenv0 (Forall origin (Var x k) t) t2 = do
  (t1, _, tenv1) <- Instantiate.type_ tenv0 origin x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 (TypeConstructor _ "fun" :@ a' :@ b' :@ e') = do
  (a, b, e, tenv1) <- Unify.function tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 (TypeConstructor _ "fun" :@ a :@ b :@ e) t2 = do
  (a', b', e', tenv1) <- Unify.function tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 t1 t2 = Unify.type_ tenv0 t1 t2

subsumptionCheckFun
  :: TypeEnv -> Type -> Type -> Type -> Type -> Type -> Type -> K TypeEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  let
    labels = effectList $ Zonk.type_ tenv2 e
    labels' = effectList $ Zonk.type_ tenv2 e'
  forM_ labels $ \ (origin, label) -> case find ((label ==) . snd) labels' of
    Just{} -> return ()
    Nothing -> report $ Report Error
      [ Item (Type.origin e)
        ["I can't match the effect type", Pretty.quote e]
      , Item (Type.origin e')
        $ ["with the effect type", Pretty.quote e']
      , Item origin
        ["because the effect label", Pretty.quote label, "is missing"]
      ]
  return tenv2
  where

  effectList :: Type -> [(Origin, Constructor)]
  effectList (TypeConstructor _ "join" :@ TypeConstructor origin label :@ es)
    = (origin, label) : effectList es
  effectList _ = []
