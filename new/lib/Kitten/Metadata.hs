{-# LANGUAGE OverloadedStrings #-}

module Kitten.Metadata
  ( Metadata(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Name (GeneralName, Unqualified)
import Kitten.Origin (Origin)
import Kitten.Term (Term)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Text.PrettyPrint as Pretty

data Metadata = Metadata
  { fields :: !(HashMap Unqualified (Term ()))
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
