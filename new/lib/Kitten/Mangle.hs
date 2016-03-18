{-# LANGUAGE OverloadedStrings #-}

module Kitten.Mangle
  ( name
  ) where

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
name qualified args = Text.concat . (["_Z", lengthEncode qualified] ++)
  $ if null args then [] else ["I", Text.concat $ map type_ args, "E"]

type_ :: Type -> Text
type_ (TypeConstructor _ "Fun" :@ _ :@ _) = "PFvv"
type_ (TypeConstructor _ "Ptr" :@ a) = Text.concat ["P", type_ a]
type_ (TypeConstructor _origin (Constructor con)) = case con of
  Qualified qualifier unqualified
    | qualifier == Vocabulary.global -> case unqualified of
      "Int32" -> "i"
      "Bool" -> "b"
      "Char" -> "c"
      "Float64" -> "f"
      "Text" -> "PKc"
      _ -> lengthEncode con
  _ -> lengthEncode con
type_ (TypeConstructor _ "Prod" :@ a :@ b)
  = Text.concat $ map type_ [a, b]
type_ t = Text.concat ["(", Text.pack (Pretty.render (pPrint t)), ")"]

lengthEncode :: Qualified -> Text
lengthEncode (Qualified (Qualifier parts) (Unqualified unqualified))
  = case parts of
    [] -> lengthEncode' unqualified
    [""] -> lengthEncode' unqualified
    _ -> mconcat $ "N" : map lengthEncode' (parts ++ [unqualified]) ++ ["E"]
  where
  lengthEncode' part = mconcat [Text.pack $ show $ Text.length part, part]
