{-# LANGUAGE OverloadedStrings #-}

module Kitten.Pretty
  ( angles
  , asDefinition
  , list
  , quote
  , vsep
  ) where

import Data.List (intersperse)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty(..))

angles :: Doc -> Doc
angles doc = hcat [char '<', doc, char '>']

list :: [Doc] -> Doc
list = hcat . intersperse ", "

quote :: (Pretty a) => a -> Doc
quote = quotes . pPrint

vsep :: [Doc] -> Doc
vsep = vcat . intersperse ""

asDefinition :: Doc -> Doc -> Doc-> Doc -> Doc
asDefinition name signature body keyword = vcat
  [ hcat
    [hsep [keyword, name, signature], ":"]
  , nest 4 body
  ]
