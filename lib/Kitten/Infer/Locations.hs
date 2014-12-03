{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( DiagnosticLocations(..)
  ) where

import Data.Text (Text)

import qualified Data.Foldable as F

import Kitten.Location
import Kitten.Kind
import Kitten.Type
import Kitten.Util.Text (ToText(..))

-- | A list of locations and associated types, suitable for
-- presenting to the end user for diagnostic purposes (e.g.
-- type errors).
class DiagnosticLocations a where
  diagnosticLocations :: Type a -> [(Location, Text)]

instance DiagnosticLocations Scalar where
  diagnosticLocations = scalarLocations

instance DiagnosticLocations Stack where
  diagnosticLocations = stackLocations

scalarLocations :: Type Scalar -> [(Location, Text)]
scalarLocations type_ = case type_ of
  TyApply a bs _ -> recur a ++ F.foldMap recur bs
  TyConst _ loc -> yield loc
  TyCtor _ loc -> yield loc
  TyFunction r s loc -> yield loc ++ stackLocations r ++ stackLocations s
  TyOption a loc -> yield loc ++ recur a
  TyProduct a b loc -> yield loc ++ recur a ++ recur b
  TyQuantified (Forall _ _ a) loc -> yield loc ++ recur a
  TySum a b loc -> yield loc ++ recur a ++ recur b
  TyVar _ loc -> yield loc
  TyVector a loc -> yield loc ++ recur a
  where
  yield loc = [(loc, toText type_)]
  recur = scalarLocations

stackLocations :: Type Stack -> [(Location, Text)]
stackLocations type_ = case type_ of
  TyStack a b -> stackLocations a ++ scalarLocations b
  TyEmpty loc -> yield loc
  TyConst _ loc -> yield loc
  TyVar _ loc -> yield loc
  where
  yield loc = [(loc, toText type_)]
