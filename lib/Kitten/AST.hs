{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Kitten.AST
  ( AST(..)
  ) where

class(Eq a, Show a, Eq (TermValue a), Show (TermValue a)) => AST a where
  type TermValue a
