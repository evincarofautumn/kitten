{-|
Module      : Kitten.Quantify
Description : Quantifying generic terms
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Quantify
  ( term
  ) where

import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..))
import Kitten.Type (Type(..), Var(..))
import qualified Kitten.Kind as Kind

-- | Copies the top-level generic value-kinded type quantifiers from a polytype
-- to an expression, thereby making the expression generic, e.g.:
--
-- > dup, ∀α:ρ. ∀β:*. ∀γ:ε. (α × β → α × β × β) ε
-- >
-- > Λβ:*. dup

term :: Type -> Sweet 'Typed -> Sweet 'Typed
term (Forall origin (Var name x Kind.Value) t) e
  = SGeneric origin name x (term t e)
term (Forall _ _ t) e = term t e
term _ e = e
