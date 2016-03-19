{-# LANGUAGE OverloadedStrings #-}

module Kitten.Mangle
  ( name
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import Kitten.Name (Qualified(..), Qualifier(..), Unqualified(..))
import Kitten.Type (Constructor(..), Type(..))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

-- Names are mangled according to the local C++ mangling convention. This is a
-- silly approximation of the IA-64 convention for testing purposes.

name :: Qualified -> [Type] -> Text
name n args = Text.pack $ Pretty.render $ Pretty.hcat
  $ pPrint n : if null args
    then [""]
    else "<" : intersperse "," (map pPrint args) ++ [">"]

type_ :: Type -> Text
type_  = Text.pack . Pretty.render . pPrint
