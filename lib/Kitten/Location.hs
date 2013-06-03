{-# LANGUAGE RecordWildCards #-}

module Kitten.Location
  ( Location(..)
  ) where

import Data.List

import Text.Parsec.Pos

data Location
  = Location
    { locationStart :: SourcePos
    , locationIndent :: Column
    }
  | UnknownLocation
  | GeneratedLocation
  | TestLocation

instance Eq Location where
  Location start1 indent1
    == Location start2 indent2
    = (start1, indent1) == (start2, indent2)
  TestLocation == _ = True
  _ == TestLocation = True
  _ == _ = False

-- Location < UnknownLocation < GeneratedLocation
instance Ord Location where
  compare
    (Location start1 indent1)
    (Location start2 indent2)
    = compare (start1, indent1) (start2, indent2)
  TestLocation `compare` _ = EQ
  _ `compare` TestLocation = EQ
  Location _ _ `compare` _ = GT
  _ `compare` Location _ _ = LT
  UnknownLocation `compare` UnknownLocation = EQ
  UnknownLocation `compare` _ = GT
  _ `compare` UnknownLocation = LT
  GeneratedLocation `compare` GeneratedLocation = EQ

instance Show Location where
  show Location{..} = intercalate ":"
    [ sourceName locationStart
    , show $ sourceLine locationStart
    , show $ sourceColumn locationStart
    ]
  show UnknownLocation = "(unknown)"
  show GeneratedLocation = "(generated)"
  show TestLocation = ""
