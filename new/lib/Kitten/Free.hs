module Kitten.Free
  ( tvs
  , tvks
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Kitten.Kind (Kind)
import Kitten.Type (Type(..), TypeId, Var(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- The free variables of a type are those not bound by any quantifier.

tvs :: Type -> Set TypeId
tvs = Set.fromList . Map.keys . tvks

tvks :: Type -> Map TypeId Kind
tvks t = case t of
  TypeConstructor{} -> Map.empty
  TypeVar _ (Var x k) -> Map.singleton x k
  TypeConstant{} -> Map.empty
  Forall _ (Var x _) t' -> Map.delete x $ tvks t'
  a :@ b -> Map.union (tvks a) (tvks b)
  TypeValue{} -> Map.empty
