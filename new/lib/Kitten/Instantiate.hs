{-# LANGUAGE OverloadedStrings #-}

module Kitten.Instantiate
  ( prenex
  , term
  , type_
  ) where

import Data.Foldable (foldlM)
import Kitten.Informer (Informer(..))
import Kitten.Kind (Kind)
import Kitten.Monad (K)
import Kitten.Origin (Origin)
import Kitten.Term (Term(..))
import Kitten.Type (Type(..), TypeId, Var(..))
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Substitute as Substitute
import qualified Kitten.Zonk as Zonk
import qualified Text.PrettyPrint as Pretty

-- To instantiate a type scheme, we simply replace all quantified variables with
-- fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order. Because type identifiers are globally
-- unique, we know a fresh type variable will never be erroneously captured.

type_ :: TypeEnv -> Origin -> TypeId -> Kind -> Type -> K (Type, Type, TypeEnv)
type_ tenv0 origin x k t = do
  ia <- freshTypeId tenv0
  let a = TypeVar origin $ Var ia k
  replaced <- Substitute.type_ tenv0 x a t
  return (replaced, a, tenv0)

-- When generating an instantiation of a generic definition, we only want to
-- instantiate the rank-1 quantifiers; all other quantifiers are irrelevant.

prenex :: TypeEnv -> Type -> K (Type, [Type], TypeEnv)
prenex tenv0 q@(Forall origin (Var x k) t)
  = while (Pretty.hsep ["instantiating", Pretty.quote q]) origin $ do
    (t', a, tenv1) <- type_ tenv0 origin x k t
    (t'', as, tenv2) <- prenex tenv1 t'
    return (t'', a : as, tenv2)
prenex tenv0 t = return (t, [], tenv0)

-- Instantiates a generic expression with the given type arguments.

term :: TypeEnv -> Term Type -> [Type] -> K (Term Type)
term tenv t args = foldlM go t args
  where
  go (Generic x expr _origin) arg = Substitute.term tenv x arg expr
  go _ _ = do
    report $ Report.TypeArgumentCountMismatch t $ map (Zonk.type_ tenv) args
    halt
