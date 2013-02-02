module Kitten.Def
  ( Def(..)
  ) where

import Data.Text (Text)

import qualified Data.Text as Text

data Def a = Def
  { defName :: !Text
  , defTerm :: !a
  }

instance (Show a) => Show (Def a) where
  show (Def name body) = unwords ["def", Text.unpack name, show body]
