module Kitten.Quantify
  ( term
  ) where

import Kitten.Kind (Kind(..))
import Kitten.Term (Term(..))
import Kitten.Type (Type(..), Var(..))

-- Copies the top-level generic value-kinded type quantifiers from a polytype to
-- an expression, thereby making the expression generic, e.g.:
--
--     ∀α:ρ. ∀β:*. ∀γ:Ε. (α × β → α × β × β) Ε    dup
--
--     Λβ:*. dup

term :: Type -> Term a -> Term a
term (Forall origin (Var x Value) t) e = Generic x (term t e) origin
term (Forall _ _ t) e = term t e
term _ e = e
