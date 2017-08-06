{-|
Module      : Kitten.Element
Description : Top-level program elements
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Kitten.Element
  ( Element(..)
  ) where

import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Phase (Phase)
import Kitten.Synonym (Synonym)
import Kitten.Term (Sweet)
import Kitten.TypeDefinition (TypeDefinition)

-- | A top-level program element.

data Element (p :: Phase)
  -- | @intrinsic@, @trait@
  = Declaration !Declaration
  -- | @define@, @instance@
  | Definition !(Definition p)
  -- | @about@
  | Metadata !Metadata
  -- | @synonym@
  | Synonym !Synonym
  -- | Top-level (@main@) code.
  | Term !(Sweet p)
  -- | @type@
  | TypeDefinition !TypeDefinition
