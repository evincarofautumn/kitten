{-# LANGUAGE OverloadedStrings #-}

module Kitten.Pretty
  ( angles
  , asDefinition
  , list
  , oxford
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

oxford :: Doc -> [Doc] -> Doc
oxford conjunction = go
  where
  go :: [Doc] -> Doc
  go [] = ""
  go [x] = x
  go [x, y] = hsep [x, conjunction, y]
  go [x, y, z] = hcat [x, ", ", y, ", ", conjunction, " ", z]
  go (x : xs) = hcat [x, ", ", go xs]

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
