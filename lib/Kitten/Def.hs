{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Kitten.Def
  ( Def(..)
  ) where

import Data.Foldable (Foldable)
import Data.Text (Text)
import Data.Traversable (Traversable)

import Kitten.Anno (Anno)
import Kitten.Location
import Kitten.Operator
import Kitten.Type (Scheme)

data Def a = Def
  { defAnno :: !Anno
  , defFixityHint :: !FixityHint
  , defLocation :: !Location
  , defName :: !Text
  , defTerm :: !(Scheme a)
  } deriving (Eq, Foldable, Functor, Show, Traversable)
