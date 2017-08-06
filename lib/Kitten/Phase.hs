module Kitten.Phase
  ( Phase(..)
  ) where

data Phase
  = Parsed    -- ^ Raw parsing result.
  | Resolved  -- ^ Names resolved to fully qualified names.
  | Postfix   -- ^ Infix operators desugared to postfix.
  | Scoped    -- ^ Quotations and locals desugared to explicit capture.
  | Typed     -- ^ Types checked and annotated.
