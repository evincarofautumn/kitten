{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Name
  ( Name(..)
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

nameIndex :: Name -> Int
nameIndex (Name index) = index
