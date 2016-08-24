{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Indent
  ( Indent(..)
  ) where

import Text.Parsec (Column)

newtype Indent = Indent Column
  deriving (Eq, Ord, Show)
