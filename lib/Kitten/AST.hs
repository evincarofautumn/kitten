{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.AST
  ( AST(..)
  ) where

class (Eq a, Show a, Eq (TermValue a), Show (TermValue a)) => AST a where
  type TermValue a
