{-|
Module      : Kitten.Quantify
Description : Quantifying generic terms
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Quantify
  ( term
  ) where

import Kitten.Term (Term(..))
import Kitten.Type (Type(..), Var(..))
import qualified Kitten.Kind as Kind

-- | Copies the top-level generic value-kinded type quantifiers from a polytype
-- to an expression, thereby making the expression generic, e.g.:
--
-- > dup, ∀α:ρ. ∀β:*. ∀γ:ε. (α × β → α × β × β) ε
-- >
-- > Λβ:*. dup

term :: Type -> Term a -> Term a
term (Forall origin (Var x Kind.Value) t) e = Generic x (term t e) origin
term (Forall _ _ t) e = term t e
term _ e = e
