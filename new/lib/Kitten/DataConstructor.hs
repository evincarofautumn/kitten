{-|
Module      : Kitten.DataConstructor
Description : Constructors of data types
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.DataConstructor
  ( DataConstructor(..)
  ) where

import Kitten.Name (Unqualified)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

-- | A single data constructor case, e.g., @case some (T)@.

data DataConstructor = DataConstructor
  { fields :: [Signature]
  , name :: !Unqualified
  , origin :: !Origin
  } deriving (Show)

-- FIXME: Support fields.
instance Pretty DataConstructor where
  pPrint constructor = "case"
    Pretty.<+> pPrint (name constructor)
