{-|
Module      : Kitten.Entry.Parent
Description : Links to parent entries
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Entry.Parent
  ( Parent(..)
  ) where

import Kitten.Name (Qualified)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Text.PrettyPrint as Pretty

-- | A parent trait (of an instance) or data type (of a constructor).

data Parent
  = Trait !Qualified
  | Type !Qualified
  deriving (Show)

instance Pretty Parent where
  pPrint parent = Pretty.hsep $ case parent of
    Trait name -> ["trait", Pretty.quote name]
    Type name -> ["type", Pretty.quote name]
