module Def
  ( Def(..)
  ) where

data Def a = Def String a

instance (Show a) => Show (Def a) where
  show (Def name body) = unwords ["def", name, show body]
