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

newtype Indent = Indent Column
  deriving (Eq, Ord, Show)
