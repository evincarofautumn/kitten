{-# LANGUAGE DeriveFunctor #-}

module Kitten.Def
  ( Def(..)
  ) where

import Data.Text (Text)

import Kitten.Anno (Anno)
import Kitten.Location
import Kitten.Type (Scheme)

data Def a = Def
  { defAnno :: !(Maybe Anno)
  , defLocation :: !Location
  , defName :: !Text
  , defTerm :: !(Scheme a)
  } deriving (Eq, Functor, Show)
