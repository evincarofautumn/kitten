{-|
Module      : Kitten.Indent
Description : Indent levels
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Indent
  ( Indent(..)
  ) where

import Text.Parsec (Column)

-- | The indent level of a token, defined as the first column of the first token
-- in the same line.

newtype Indent = Indent Column
  deriving (Eq, Ord, Show)
