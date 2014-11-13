{-# LANGUAGE OverloadedStrings #-}

module Kitten.Name
  ( MixfixNamePart(..)
  , Name(..)
  , Qualifier(..)
  ) where

import GHC.Exts
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Util.Text (ToText(..))

data Name
  = MixfixName [MixfixNamePart]
  | Qualified Qualifier Text
  | Unqualified Text
  deriving (Eq, Ord)

data MixfixNamePart
  = MixfixNamePart !Text
  | MixfixHole
  deriving (Eq, Ord)

data Qualifier = Qualifier !(Vector Text)
  deriving (Eq, Ord)

instance Hashable Name where
  hashWithSalt salt (MixfixName parts)
    = hashWithSalt salt (0::Int, parts)
  hashWithSalt salt (Qualified qualifier name)
    = hashWithSalt salt (1::Int, qualifier, name)
  hashWithSalt salt (Unqualified name)
    = hashWithSalt salt (2::Int, name)

instance Hashable MixfixNamePart where
  hashWithSalt salt (MixfixNamePart name)
    = hashWithSalt salt (0::Int, name)
  hashWithSalt salt (MixfixHole)
    = hashWithSalt salt (1::Int)

instance Hashable Qualifier where
  -- FIXME Why doesn't Vector have a Hashable instance?
  hashWithSalt salt (Qualifier qualifier)
    = hashWithSalt salt (0::Int, V.toList qualifier)

instance ToText Name where
  toText (MixfixName parts)
    = T.concat (map toText parts)
  toText (Qualified (Qualifier qualifier) name)
    = T.intercalate "_" (V.toList qualifier ++ [name])
  toText (Unqualified name) = name

instance ToText MixfixNamePart where
  toText (MixfixNamePart name) = name
  toText MixfixHole = "()"

instance Show Name where
  show = T.unpack . toText

instance IsString Name where
  fromString = Unqualified . T.pack

instance IsString MixfixNamePart where
  fromString = MixfixNamePart . T.pack
