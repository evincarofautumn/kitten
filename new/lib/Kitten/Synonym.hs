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
