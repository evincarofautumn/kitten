{-|
Module      : Kitten.Substitute
Description : Substituting type variables
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Substitute
  ( term
  , type_
  ) where

import Kitten.Monad (K)
import Kitten.Term (Case(..), Else(..), Term(..))
import Kitten.Type (Type(..), TypeId, Var(..))
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import qualified Data.Set as Set
import qualified Kitten.Free as Free
import qualified Kitten.Kind as Kind

-- | Capture-avoiding substitution of a type variable α with a type τ throughout
-- a type σ, [α ↦ τ]σ.

type_ :: TypeEnv -> TypeId -> Type -> Type -> K Type
type_ tenv0 x a = recur
  where
  recur t = case t of
    Forall origin (Var x' k) t'
      | x == x' -> return t
      | x' `Set.notMember` Free.tvs tenv0 t' -> Forall origin (Var x' k) <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- type_ tenv0 x' (TypeVar origin $ Var z k) t'
        Forall origin (Var z k) <$> recur t''
    TypeVar _ (Var x' _) | x == x' -> return a
    m :@ n -> (:@) <$> recur m <*> recur n
    _ -> return t

term :: TypeEnv -> TypeId -> Type -> Term Type -> K (Term Type)
term tenv x a = recur
  where
  recur t = case t of
    Call tref origin -> Call <$> go tref <*> pure origin
    Coercion hint tref origin -> Coercion hint <$> go tref <*> pure origin
    Compose tref t1 t2 -> Compose <$> go tref <*> recur t1 <*> recur t2
    Generic x' body origin -> do
      -- FIXME: Generics could eventually quantify over non-value kinds.
      let k = Kind.Value
      z <- freshTypeId tenv
      body' <- term tenv x' (TypeVar origin $ Var z k) body
      Generic z <$> recur body' <*> pure origin
    Group body -> recur body
    Lambda tref name varType body origin -> Lambda <$> go tref
      <*> pure name <*> go varType <*> recur body <*> pure origin
    Match hint tref cases else_ origin -> Match hint <$> go tref
      <*> mapM goCase cases <*> goElse else_ <*> pure origin
      where

      goCase :: Case Type -> K (Case Type)
      goCase (Case name body caseOrigin)
        = Case name <$> recur body <*> pure caseOrigin

      goElse :: Else Type -> K (Else Type)
      goElse (Else body elseOrigin) = Else <$> recur body <*> pure elseOrigin

    New tref index size origin -> New
      <$> go tref <*> pure index <*> pure size <*> pure origin
    NewClosure tref size origin -> NewClosure <$> go tref
      <*> pure size <*> pure origin
    NewVector tref size elemType origin -> NewVector <$> go tref
      <*> pure size <*> go elemType <*> pure origin
    Push tref value origin -> Push <$> go tref <*> pure value <*> pure origin
    Word tref fixity name args origin -> Word <$> go tref
      <*> pure fixity <*> pure name <*> mapM go args <*> pure origin

  go = type_ tenv x a
