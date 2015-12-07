{-# LANGUAGE OverloadedStrings #-}

module Kitten.Regeneralize
  ( regeneralize
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Function (on)
import Data.List (deleteBy)
import Kitten.Kind (Kind)
import Kitten.Occurrences (occurrences)
import Kitten.Type (Type(..), TypeId, Var(..), funType)
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Kitten.Free as Free
import qualified Kitten.Type as Type

-- Because all functions are polymorphic with respect to the part of the stack
-- they don't touch, all words of order n can be regeneralized to words of rank
-- n with respect to the stack-kinded type variables.
--
-- This means that if a stack-kinded (ρ) type variable occurs only twice in a
-- type, in the bottommost position on both sides of a function arrow, then its
-- scope can be reduced to only that function arrow by introducing a
-- higher-ranked quantifier. This is a more conservative rule than used in
-- "Simple type inference for higher-order stack languages". For example, the
-- type of "map":
--
--     map :: ∀ρσαβ. ρ × vector α × (σ × α → σ × β) → ρ × vector β
--
-- Can be regeneralized like so:
--
--     map :: ∀ραβ. ρ × vector α × (∀σ. σ × α → σ × β) → ρ × vector β
--
-- In order to correctly regeneralize a type, it needs to contain no
-- higher-ranked quantifiers.

regeneralize :: TypeEnv -> Type -> Type
regeneralize tenv t = let
  (t', vars) = runWriter $ go t
  in foldr (uncurry ((Forall (Type.origin t) .) . Var)) t'
    $ foldr (deleteBy ((==) `on` fst)) (Map.toList (Free.tvks t')) vars
  where
  go :: Type -> Writer [(TypeId, Kind)] Type
  go t' = case t' of
    TypeConstructor _ "fun" :@ a :@ b :@ e
      | TypeVar origin (Var c k) <- bottommost a
      , TypeVar _ (Var d _) <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [(c, k)]
        a' <- go a
        b' <- go b
        e' <- go e
        return $ Forall origin (Var c k) $ funType origin a' b' e'
    c@(TypeConstructor _ "prod") :@ a :@ b -> do
      a' <- go a
      b' <- go b
      return $ c :@ a' :@ b'
    -- FIXME: This should descend into the quantified type.
    Forall{} -> return t'
    a :@ b -> (:@) <$> go a <*> go b
    _ -> return t'

bottommost :: Type -> Type
bottommost (TypeConstructor _ "prod" :@ a :@ _) = bottommost a
bottommost a = a
