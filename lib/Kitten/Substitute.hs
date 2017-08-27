{-|
Module      : Kitten.Substitute
Description : Substituting type variables
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Kitten.Substitute
  ( term
  , type_
  ) where

import Kitten.Monad (K)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..))
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
    Forall origin var@(Var name x' k) t'
      | x == x' -> return t
      | x' `Set.notMember` Free.tvs tenv0 t' -> Forall origin var <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- type_ tenv0 x' (TypeVar origin $ Var name z k) t'
        Forall origin (Var name z k) <$> recur t''
    TypeVar _ (Var _name x' _) | x == x' -> return a
    m :@ n -> (:@) <$> recur m <*> recur n
    _ -> return t

term :: TypeEnv -> TypeId -> Type -> Sweet 'Typed -> K (Sweet 'Typed)
term tenv x a = recur
  where
  recur t = case t of

    SArray tref origin items -> do
      tref' <- go tref
      SArray tref' origin <$> mapM recur items

    SAs tref origin types -> do
      tref' <- go tref
      pure $ SAs tref' origin types

    SCharacter tref origin text -> do
      tref' <- go tref
      pure $ SCharacter tref' origin text

    SCompose tref t1 t2 -> do
      tref' <- go tref
      SCompose tref' <$> recur t1 <*> recur t2

    SDo tref origin t1 t2 -> do
      tref' <- go tref
      SDo tref' origin <$> recur t1 <*> recur t2

    SEscape tref origin body -> do
      tref' <- go tref
      SEscape tref' origin <$> recur body

    SFloat tref origin literal -> do
      tref' <- go tref
      pure $ SFloat tref' origin literal

    SGeneric origin name x' body -> do
      -- FIXME: Generics could eventually quantify over non-value kinds.
      let k = Kind.Value
      z <- freshTypeId tenv
      body' <- term tenv x' (TypeVar origin $ Var name z k) body
      SGeneric origin name z <$> recur body'

    SGroup tref origin body -> do
      tref' <- go tref
      SGroup tref' origin <$> recur body

    SIdentity tref origin -> do
      tref' <- go tref
      pure $ SIdentity tref' origin

    SIf tref origin mCondition true elifs mElse -> do
      tref' <- go tref
      mCondition' <- traverse recur mCondition
      true' <- recur true
      elifs' <- traverse (\ (elifOrigin, condition, body)
        -> (,,) elifOrigin <$> recur condition <*> recur body) elifs
      mElse' <- traverse recur mElse
      pure $ SIf tref' origin mCondition' true' elifs' mElse'

    SInfix tref origin left op right typeArgs -> do
      tref' <- go tref
      SInfix tref' origin <$> recur left <*> pure op <*> recur right
        <*> traverse go typeArgs

    SInteger tref origin literal -> do
      tref' <- go tref
      pure $ SInteger tref' origin literal

    SJump tref origin -> do
      tref' <- go tref
      pure $ SJump tref' origin

    SLambda tref origin vars body -> do
      tref' <- go tref
      vars' <- traverse (\ (varOrigin, mName, varType)
        -> (,,) varOrigin mName <$> go varType) vars
      SLambda tref' origin vars' <$> recur body

    SList tref origin items -> do
      tref' <- go tref
      SList tref' origin <$> mapM recur items

    SLocal tref origin name -> do
      tref' <- go tref
      pure $ SLocal tref' origin name

    SLoop tref origin -> do
      tref' <- go tref
      pure $ SLoop tref' origin

    SMatch tref origin mScrutinee cases mElse -> do
      tref' <- go tref
      mScrutinee' <- traverse recur mScrutinee
      cases' <- traverse (\ (caseOrigin, name, body)
        -> (,,) caseOrigin name <$> recur body) cases
      mElse' <- traverse recur mElse
      pure $ SMatch tref' origin mScrutinee' cases' mElse'

    SNestableCharacter tref origin text -> do
      tref' <- go tref
      pure $ SNestableCharacter tref' origin text

    SNestableText tref origin text -> do
      tref' <- go tref
      pure $ SNestableText tref' origin text

    SPack tref origin boxed vars hiddenType -> do
      tref' <- go tref
      vars' <- mapM (\ (varType, name) -> (,) <$> go varType <*> pure name) vars
      hiddenType' <- go hiddenType
      pure $ SPack tref' origin boxed vars' hiddenType'

    SParagraph tref origin text -> do
      tref' <- go tref
      pure $ SParagraph tref' origin text

    STag tref origin size index -> do
      tref' <- go tref
      pure $ STag tref' origin size index

    SText tref origin text -> do
      tref' <- go tref
      pure $ SText tref' origin text

    SQuotation tref origin body -> do
      tref' <- go tref
      SQuotation tref' origin <$> recur body

    SReturn tref origin -> do
      tref' <- go tref
      pure $ SReturn tref' origin

    SSection tref origin name swap operand typeArgs -> do
      tref' <- go tref
      SSection tref' origin name swap <$> recur operand
        <*> traverse go typeArgs

    STodo tref origin -> do
      tref' <- go tref
      pure $ STodo tref' origin

    SUnboxedQuotation tref origin body -> do
      tref' <- go tref
      SUnboxedQuotation tref' origin <$> recur body

    SWith tref origin permits -> do
      tref' <- go tref
      pure $ SWith tref' origin permits

    SWord tref origin fixity name typeArgs -> do
      tref' <- go tref
      SWord tref' origin fixity name <$> traverse go typeArgs

  go = type_ tenv x a
