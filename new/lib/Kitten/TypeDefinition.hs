{-# LANGUAGE OverloadedStrings #-}

module Kitten.TypeDefinition
  ( TypeDefinition(..)
  ) where

import Kitten.DataConstructor (DataConstructor)
import Kitten.Entry.Parameter (Parameter)
import Kitten.Name (Qualified)
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

data TypeDefinition = TypeDefinition
  { constructors :: [DataConstructor]
  , name :: !Qualified
  , origin :: !Origin
  , parameters :: [Parameter]
  } deriving (Show)

-- FIXME: Support parameters.
instance Pretty TypeDefinition where
  pPrint definition = Pretty.vcat
    [ "type"
      Pretty.<+> pPrint (name definition)
      Pretty.<> ":"
    , Pretty.nest 4 $ Pretty.vcat $ map pPrint $ constructors definition
    ]
