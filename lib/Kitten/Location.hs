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

instance Show Location where
  show Location{..} = intercalate ":"
    [ sourceName locationStart
    , show $ sourceLine locationStart
    , show $ sourceColumn locationStart
    ]
  show UnknownLocation = "(unknown)"
  show GeneratedLocation = "(generated)"
  show TestLocation = ""
