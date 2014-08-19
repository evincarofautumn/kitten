{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( diagnosticLocations
  ) where

import Data.Text (Text)

import qualified Data.Foldable as F

import Kitten.Util.Text (ToText(..))
import Kitten.Types

-- | A list of locations and associated types, suitable for
-- presenting to the end user for diagnostic purposes (e.g.
-- type errors).
diagnosticLocations
  :: (ToText (Type a)) => Type a -> [(Origin, Text)]
diagnosticLocations type_ = case type_ of
  a :& b -> locations a ++ locations b
  a :. b -> locations a ++ locations b
  (:?) a -> locations a
  a :@ bs -> locations a ++ F.foldMap locations bs
  a :| b -> locations a ++ locations b
  TyConst _ loc -> yield loc
  TyCtor _ loc -> yield loc
  TyEmpty loc -> yield loc
  TyFunction r s loc -> yield loc ++ locations r ++ locations s
  TyQuantified _ loc -> yield loc
  TyVar _ loc -> yield loc
  TyVector a loc -> yield loc ++ locationsIfUnhinted loc a

  where
  yield origin = [(origin, toText type_)]

  locations
    :: (ToText (Type a)) => Type a -> [(Origin, Text)]
  locations = diagnosticLocations

  locationsIfUnhinted
    :: (ToText (Type a)) => Origin -> Type a -> [(Origin, Text)]
  locationsIfUnhinted (Origin HiNone _) = locations
  locationsIfUnhinted (Origin _ _) = const []
