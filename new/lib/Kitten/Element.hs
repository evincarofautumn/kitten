{-|
Module      : Kitten.Element
Description : Top-level program elements
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Element
  ( Element(..)
  ) where

import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Synonym (Synonym)
import Kitten.Term (Term)
import Kitten.TypeDefinition (TypeDefinition)

data Element a
  = Declaration !Declaration
  | Definition !(Definition a)
  | Metadata !Metadata
  | Synonym !Synonym
  | Term !(Term a)
  | TypeDefinition !TypeDefinition
