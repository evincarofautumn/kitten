{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( locations
  ) where

import Kitten.Location
import Kitten.Type
import Kitten.Util.Text (Textable(..), ToText)

locations
  :: (ToText (Type a)) => Type a -> [(Location, Textable)]
locations type_ = case type_ of
  a :& b -> locations a ++ locations b
  a :. b -> locations a ++ locations b
  (:?) a -> locations a
  a :| b -> locations a ++ locations b
  Bool loc -> yield loc
  Char loc -> yield loc
  Empty loc -> yield loc
  Float loc -> yield loc
  Function r s e loc
    -> yield loc
    ++ locations r
    ++ locations s
    ++ locations e
  Handle loc -> yield loc
  Int loc -> yield loc
  Named _ loc -> yield loc
  Test -> []
  Unit loc -> yield loc
  Var _ loc -> yield loc
  Vector a loc -> yield loc ++ locations a

  a :+ b -> locations a ++ locations b
  NoEffect loc -> yield loc
  IOEffect loc -> yield loc

  where
  yield :: Location -> [(Location, Textable)]
  yield loc = [(loc, Textable type_)]
