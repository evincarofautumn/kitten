{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Name
  ( Name(..)
  , NameGen
  , genName
  , mkNameGen
  , mkNameGenFrom
  , nameIndex
  ) where

import Data.Monoid

import qualified Data.Text as T

import Kitten.Util.Text (ToText(..), showText)

newtype Name = Name Int
  deriving (Enum, Eq, Ord)

instance Show Name where
  show = T.unpack . toText

instance ToText Name where
  toText (Name name) = "_" <> showText name

newtype NameGen = NameGen Name

genName :: NameGen -> (Name, NameGen)
genName (NameGen name) = (name, NameGen (succ name))

mkNameGen :: NameGen
mkNameGen = mkNameGenFrom (Name 0)

mkNameGenFrom :: Name -> NameGen
mkNameGenFrom = NameGen

nameIndex :: Name -> Int
nameIndex (Name index) = index
