{-|
Module      : Kitten.Zonk
Description : Fully substituting type variables
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.Zonk
  ( type_
  , term
  ) where

import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..))
import Kitten.Type (Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Kitten.TypeEnv as TypeEnv

-- | Zonking a type fully substitutes all type variables. That is, if you have:
--
-- > t0 ~ t1
-- > t1 ~ Int32
--
-- Then zonking @t0@ gives you @Int32@.

type_ :: TypeEnv -> Type -> Type
type_ tenv0 = recur
  where
  recur t = case t of
    TypeConstructor{} -> t
    TypeValue{} -> error "TODO: zonk type value"
    TypeVar _origin (Var _name x _k) -> case Map.lookup x (TypeEnv.tvs tenv0) of
      -- FIXME: Is this necessary?
      -- Just (TypeVar _origin (Var x' _)) | x == x' -> TypeVar origin (Var x k)
      Just t' -> recur t'
      Nothing -> t
    TypeConstant{} -> t
    Forall origin var@(Var _ i _) t' -> Forall origin var
      $ type_ tenv0 { TypeEnv.tvs = Map.delete i $ TypeEnv.tvs tenv0 } t'
    a :@ b -> recur a :@ recur b

-- | Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.

term :: TypeEnv -> Sweet 'Typed -> Sweet 'Typed
term tenv0 = go
  where
    zonk = type_ tenv0
    go t = case t of
      SArray tref origin items
        -> SArray (zonk tref) origin (go <$> items)
      SAs tref origin types
        -> SAs (zonk tref) origin types
      SCharacter tref origin text
        -> SCharacter (zonk tref) origin text
      SCompose tref a b
        -> SCompose (zonk tref) (go a) (go b)
      SDo tref origin f x
        -> SDo (zonk tref) origin (go f) (go x)
      SEscape tref origin body
        -> SEscape (zonk tref) origin body
      SFloat tref origin literal
        -> SFloat (zonk tref) origin literal
      -- FIXME: Should this affect zonking?
      SGeneric origin name x body
        -> SGeneric origin name x (go body)
      SGroup tref origin body
        -> SGroup (zonk tref) origin (go body)
      SIdentity tref origin
        -> SIdentity (zonk tref) origin
      SIf tref origin mCondition true elifs mElse
        -> SIf (zonk tref) origin (go <$> mCondition) (go true)
          (map (\ (elifOrigin, condition, body)
            -> (elifOrigin, go condition, go body)) elifs)
          (go <$> mElse)
      SInfix tref origin left op right typeArgs
        -- TODO: Zonk type arguments?
        -> SInfix (zonk tref) origin (go left) op (go right) typeArgs
      SInteger tref origin literal
        -> SInteger (zonk tref) origin literal
      SJump tref origin
        -> SJump (zonk tref) origin
      SLambda tref origin vars body
        -> SLambda (zonk tref) origin
          (map (\ (varOrigin, mName, varType)
            -> (varOrigin, mName, zonk varType)) vars)
          (go body)
      SList tref origin items
        -> SList (zonk tref) origin (go <$> items)
      SLocal tref origin name
        -> SLocal (zonk tref) origin name
      SLoop tref origin
        -> SLoop (zonk tref) origin
      SMatch tref origin mScrutinee cases mElse
        -> SMatch (zonk tref) origin (go <$> mScrutinee)
          (map (\ (caseOrigin, name, body)
            -> (caseOrigin, name, go body)) cases)
          (go <$> mElse)
      SNestableCharacter tref origin text
        -> SNestableCharacter (zonk tref) origin text
      SNestableText tref origin text
        -> SNestableText (zonk tref) origin text
      SParagraph tref origin text
        -> SParagraph (zonk tref) origin text
      STag tref origin size index
        -> STag (zonk tref) origin size index
      SText tref origin text
        -> SText (zonk tref) origin text
      SQuotation tref origin body
        -> SQuotation (zonk tref) origin (go body)
      SReturn tref origin
        -> SReturn (zonk tref) origin
      SSection tref origin name swap operand typeArgs
        -- TODO: Zonk type arguments?
        -> SSection (zonk tref) origin name swap (go operand) typeArgs
      STodo tref origin
        -> STodo (zonk tref) origin
      SUnboxedQuotation tref origin body
        -> SUnboxedQuotation (zonk tref) origin body
      SWith tref origin permits
        -> SWith (zonk tref) origin permits
      SWord tref origin fixity name typeArgs
        -- TODO: Zonk type arguments?
        -> SWord (zonk tref) origin fixity name typeArgs
