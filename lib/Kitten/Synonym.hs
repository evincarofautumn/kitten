{-|
Module      : Kitten.Synonym
Description : Aliases
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Synonym
  ( Synonym(..)
  ) where

import Kitten.Name (GeneralName, Qualified)
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))

data Synonym = Synonym !Qualified !GeneralName !Origin
  deriving (Show)

-- FIXME: Real instance.
instance Pretty Synonym where
  pPrint _ = "synonym"
