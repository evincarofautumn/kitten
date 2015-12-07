module Kitten.Zonk
  ( type_
  , term
  ) where

import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Kitten.TypeEnv as TypeEnv

-- Zonking a type fully substitutes all type variables. That is, if you have:
--
--     t0 ~ t1
--     t1 ~ int
--
-- Then zonking "t0" gives you "int".

type_ :: TypeEnv -> Type -> Type
type_ tenv0 = recur
  where
  recur t = case t of
    TypeConstructor{} -> t
    TypeVar _origin (Var x _k) -> case Map.lookup x (TypeEnv.tvs tenv0) of
      -- FIXME: Is this necessary?
      -- Just (TypeVar _origin (Var x' _)) | x == x' -> TypeVar origin (Var x k)
      Just t' -> recur t'
      Nothing -> t
    TypeConstant{} -> t
    Forall origin (Var x k) t' -> Forall origin (Var x k)
      $ type_ tenv0 { TypeEnv.tvs = Map.delete x $ TypeEnv.tvs tenv0 } t'
    a :@ b -> recur a :@ recur b

-- Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.

term :: TypeEnv -> Term -> Term
term tenv0 = go
  where
  go t = case t of
    Call tref fixity name params origin
      -> Call (zonkMay tref) fixity name params origin
    Compose tref a b
      -> Compose (zonkMay tref) (go a) (go b)
    Drop tref origin
      -> Drop (zonkMay tref) origin
    Generic x a origin
      -> Generic x (go a) origin
    Group a
      -> go a
    Identity tref origin
      -> Identity (zonkMay tref) origin
    If tref true false origin
      -> If (zonkMay tref) (go true) (go false) origin
    Intrinsic tref name origin
      -> Intrinsic (zonkMay tref) name origin
    Lambda tref name varType body origin
      -> Lambda (zonkMay tref) name (zonkMay varType) (go body) origin
    Match tref cases mElse origin
      -> Match (zonkMay tref) (map goCase cases) (fmap goElse mElse) origin
      where
      goCase (Case name body caseOrigin)
        = Case name (go body) caseOrigin
      goElse (Else body elseOrigin)
        = Else (go body) elseOrigin
    New tref index origin
      -> New (zonkMay tref) index origin
    NewClosure tref index origin
      -> NewClosure (zonkMay tref) index origin
    NewVector tref size origin
      -> NewVector (zonkMay tref) size origin
    Push tref value' origin
      -> Push (zonkMay tref) (value tenv0 value') origin
    Swap tref origin
      -> Swap (zonkMay tref) origin
  zonkMay = fmap $ type_ tenv0

value :: TypeEnv -> Value -> Value
value tenv0 = go
  where
  go v = case v of
    Boolean{} -> v
    Character{} -> v
    Closed{} -> v
    Closure names body -> Closure names $ term tenv0 body
    Float{} -> v
    Integer{} -> v
    Local{} -> v
    Name{} -> v
    Quotation body -> Quotation $ term tenv0 body
    Text{} -> v
