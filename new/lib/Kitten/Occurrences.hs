module Kitten.Occurrences
  ( occurrences
  , occurs
  ) where

import Kitten.Type (Type(..), TypeId, Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Kitten.TypeEnv as TypeEnv

-- We need to be able to count occurrences of a type variable in a type, not
-- just check for its presence. This is for two reasons: to prevent infinite
-- types (the "occurs check"), and to determine whether a stack variable can be
-- generalized to a higher rank.

occurrences :: TypeEnv -> TypeId -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TypeConstructor{} -> 0
    TypeVar _ (Var y _) -> case Map.lookup y (TypeEnv.tvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TypeConstant{} -> 0
    Forall _ (Var x' _) t' -> if x == x' then 0 else recur t'
    a :@ b -> recur a + recur b

occurs :: TypeEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0
