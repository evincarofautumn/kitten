module Kitten.Quantify
  ( program
  , term
  ) where

import Kitten.Kind (Kind(..))
import Kitten.Program (Program(..))
import Kitten.Term (Term(..))
import Kitten.Type (Type(..), Var(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Program as Program

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

program :: Program a -> Program a
program p = p
  { Program.definitions = HashMap.mapWithKey go $ Program.definitions p }
  where
  go (_, type_) t = term type_ t
