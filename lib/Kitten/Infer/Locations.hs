{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( diagnosticLocations
  ) where

import Kitten.Location
import Kitten.Util.Text (Textable(..), ToText)
import Kitten.Types

-- | A list of locations and associated types, suitable for
-- presenting to the end user for diagnostic purposes (e.g.
-- type errors).
diagnosticLocations
  :: (ToText (Type a)) => Type a -> [(Location, Textable)]
diagnosticLocations type_ = case type_ of
  a :& b -> locations a ++ locations b
  a :. b -> locations a ++ locations b
  (:?) a -> locations a
  a :| b -> locations a ++ locations b
  TyConst _ loc -> yield loc
  TyCtor _ loc -> yield loc
  TyEmpty loc -> yield loc
  TyFunction r s loc -> yield loc ++ locations r ++ locations s
  TyQuantified _ loc -> yield loc
  TyVar _ loc -> yield loc
  TyVector a loc -> yield loc ++ locationsIfUnhinted loc a

  where
  yield :: Origin -> [(Location, Textable)]
  yield (Origin _ loc) = [(loc, Textable type_)]

  locations
    :: (ToText (Type a)) => Type a -> [(Location, Textable)]
  locations = diagnosticLocations

  locationsIfUnhinted
    :: (ToText (Type a)) => Origin -> Type a -> [(Location, Textable)]
  locationsIfUnhinted (Origin HiNone _) = locations
  locationsIfUnhinted (Origin _ _) = const []
