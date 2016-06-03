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
--     t1 ~ Int32
--
-- Then zonking "t0" gives you "Int32".

type_ :: TypeEnv -> Type -> Type
type_ tenv0 = recur
  where
  recur t = case t of
    TypeConstructor{} -> t
    TypeValue{} -> error "TODO: zonk type value"
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

term :: TypeEnv -> Term Type -> Term Type
term tenv0 = go
  where
  zonk = type_ tenv0
  go t = case t of
    Call tref origin -> Call (zonk tref) origin
    Compose tref a b
      -> Compose (zonk tref) (go a) (go b)
    Drop tref origin
      -> Drop (zonk tref) origin
    Generic x a origin
      -> Generic x (go a) origin
    Group a
      -> go a
    Identity tref origin
      -> Identity (zonk tref) origin
    If tref true false origin
      -> If (zonk tref) (go true) (go false) origin
    Lambda tref name varType body origin
      -> Lambda (zonk tref) name (zonk varType) (go body) origin
    Match tref cases mElse origin
      -> Match (zonk tref) (map goCase cases) (fmap goElse mElse) origin
      where
      goCase (Case name body caseOrigin)
        = Case name (go body) caseOrigin
      goElse (Else body elseOrigin)
        = Else (go body) elseOrigin
    New tref index size origin
      -> New (zonk tref) index size origin
    NewClosure tref index origin
      -> NewClosure (zonk tref) index origin
    NewVector tref size elemType origin
      -> NewVector (zonk tref) size (zonk elemType) origin
    Push tref value' origin
      -> Push (zonk tref) (value tenv0 value') origin
    Swap tref origin
      -> Swap (zonk tref) origin
    With tref permits origin
      -> With (zonk tref) permits origin
    Word tref fixity name params origin
      -> Word (zonk tref) fixity name params origin

value :: TypeEnv -> Value Type -> Value Type
value tenv0 = go
  where
  go v = case v of
    Algebraic{} -> error "adt should not appear before runtime"
    Array{} -> error "array should not appear before runtime"
    Capture names body -> Capture names $ term tenv0 body
    Character{} -> v
    Closed{} -> v
    Closure{} -> v
    Float{} -> v
    Integer{} -> v
    Local{} -> v
    Name{} -> v
    Quotation body -> Quotation $ term tenv0 body
    Text{} -> v
