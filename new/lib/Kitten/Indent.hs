{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Indent
  ( Indent(..)
  ) where

import Text.Parsec (Column)

newtype Indent = Indent (Maybe Column)
  deriving (Eq, Ord, Show)
