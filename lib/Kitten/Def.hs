module Kitten.Def
  ( Def(..)
  ) where

data Def a = Def
  { defName :: String
  , defTerm :: a
  }

instance (Show a) => Show (Def a) where
  show (Def name body) = unwords ["def", name, show body]
