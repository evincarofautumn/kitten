{-# LANGUAGE OverloadedStrings #-}

module Kitten.TypeDefinition
  ( TypeDefinition(..)
) where

import Kitten.DataConstructor (DataConstructor)
import Kitten.Kind (Kind)
import Kitten.Name (Qualified, Unqualified)
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

data TypeDefinition = TypeDefinition
  { constructors :: [DataConstructor]
  , name :: !Qualified
  , origin :: !Origin
  , parameters :: [(Unqualified, Kind, Origin)]
  } deriving (Show)

-- FIXME: Support parameters.
instance Pretty TypeDefinition where
  pPrint definition = Pretty.vcat
    [ "type"
      Pretty.<+> pPrint (name definition)
      Pretty.<> ":"
    , Pretty.nest 4 $ Pretty.vcat $ map pPrint $ constructors definition
    ]
