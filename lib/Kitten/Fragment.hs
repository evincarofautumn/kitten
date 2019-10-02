{-|
Module      : Kitten.Fragment
Description : Program fragments
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Fragment
  ( Fragment(..)
  ) where

import Data.Semigroup (Semigroup (..))
import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Synonym (Synonym)
import Kitten.TypeDefinition (TypeDefinition)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty

-- | A program fragment, consisting of a bag of top-level program elements.

data Fragment a = Fragment
  { declarations :: [Declaration]
  , definitions :: [Definition a]
  , metadata :: [Metadata]
  , synonyms :: [Synonym]
  , types :: [TypeDefinition]
  } deriving (Show)

instance Monoid (Fragment a) where
  mempty = Fragment
    { declarations = []
    , definitions = []
    , metadata = []
    , synonyms = []
    , types = []
    }
  mappend = (<>)

instance Semigroup (Fragment a) where
  (<>) a b = Fragment
    { declarations = declarations a ++ declarations b
    , definitions = definitions a ++ definitions b
    , metadata = metadata a ++ metadata b
    , synonyms = synonyms a ++ synonyms b
    , types = types a ++ types b
    }

instance Pretty (Fragment a) where
  pPrint fragment = Pretty.vsep $ concat
    [ map pPrint $ definitions fragment
    , map pPrint $ metadata fragment
    , map pPrint $ synonyms fragment
    , map pPrint $ types fragment
    ]
