{-|
Module      : Kitten.Metadata
Description : Metadata about identifiers in the dictionary
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Metadata
  ( Metadata(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Name (GeneralName, Unqualified)
import Kitten.Origin (Origin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Text.PrettyPrint as Pretty

-- | Untyped metadata from @about@ blocks.

data Metadata = Metadata
  { fields :: !(HashMap Unqualified (Sweet 'Parsed))
  , name :: !GeneralName
  , origin :: !Origin
  } deriving (Show)

instance Pretty Metadata where
  pPrint metadata = Pretty.vcat
    [ Pretty.hcat ["about ", pPrint $ name metadata, ":"]
    , Pretty.nest 4 $ Pretty.vcat $ map field $ HashMap.toList
      $ fields metadata
    ]
    where
    field (key, value) = Pretty.vcat
      [ Pretty.hcat [pPrint key, ":"]
      , Pretty.nest 4 $ pPrint value
      ]
