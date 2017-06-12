{-|
Module      : Kitten.Free
Description : Free variables of a type
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Free
  ( tvs
  , tvks
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Kitten.Kind (Kind)
import Kitten.Type (Type(..), TypeId, Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Kitten.Zonk as Zonk

-- | Just the free variables of a type, without their kinds.

tvs :: TypeEnv -> Type -> Set TypeId
tvs tenv0 = Set.fromList . Map.keys . tvks tenv0

-- | Finds free variables (those not bound by any quantifier) and returns them
-- along with their kinds.

tvks :: TypeEnv -> Type -> Map TypeId Kind
tvks tenv = go . Zonk.type_ tenv
  where
  go t = case t of
    TypeConstructor{} -> Map.empty
    TypeVar _ (Var x k) -> Map.singleton x k
    TypeConstant{} -> Map.empty
    Forall _ (Var x _) t' -> Map.delete x $ go t'
    a :@ b -> Map.union (go a) (go b)
    TypeValue{} -> Map.empty
