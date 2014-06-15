{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( DiagnosticLocations(..)
  ) where

import Data.Text (Text)

import qualified Data.Foldable as F

import Kitten.Types
import Kitten.Util.Text (ToText(..))

-- | A list of locations and associated types, suitable for
-- presenting to the end user for diagnostic purposes (e.g.
-- type errors).
class DiagnosticLocations a where
  diagnosticLocations :: Type a -> [(Origin, Text)]

instance DiagnosticLocations Scalar where
  diagnosticLocations = scalarLocations

instance DiagnosticLocations Stack where
  diagnosticLocations = stackLocations

scalarLocations :: Type Scalar -> [(Origin, Text)]
scalarLocations type_ = case type_ of
  a :& b -> locations a ++ locations b
  (:?) a -> locations a
  a :@ bs -> locations a ++ F.foldMap locations bs
  a :| b -> locations a ++ locations b
  TyConst _ loc -> yield loc
  TyCtor _ loc -> yield loc
  TyFunction r s loc -> yield loc ++ stackLocations r ++ stackLocations s
  TyQuantified _ loc -> yield loc
  TyVar _ loc -> yield loc
  TyVector a loc -> yield loc ++ locationsIfUnhinted loc a

  where
  yield origin = [(origin, toText type_)]

  locations :: Type Scalar -> [(Origin, Text)]
  locations = scalarLocations

  locationsIfUnhinted :: Origin -> Type Scalar -> [(Origin, Text)]
  locationsIfUnhinted (Origin HiNone _) = locations
  locationsIfUnhinted (Origin _ _) = const []

stackLocations :: Type Stack -> [(Origin, Text)]
stackLocations type_ = case type_ of
  a :. b -> stackLocations a ++ scalarLocations b
  _ -> []
