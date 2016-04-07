{-# LANGUAGE OverloadedStrings #-}

module Kitten.Entry.Parent
  ( Parent(..)
  ) where

import Kitten.Name (Qualified)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Text.PrettyPrint as Pretty

data Parent
  = Trait !Qualified
  | Type !Qualified
  deriving (Show)

instance Pretty Parent where
  pPrint parent = Pretty.hsep $ case parent of
    Trait name -> ["trait", Pretty.quote name]
    Type name -> ["type", Pretty.quote name]
