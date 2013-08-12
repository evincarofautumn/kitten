{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Location
  ( Location(..)
  ) where

import Text.Parsec.Pos

import qualified Data.Text as T

import Kitten.Util.Text (ToText(..), showText)

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
  show = T.unpack . toText

instance ToText Location where
  toText Location{..} = T.intercalate ":"
    [ T.pack $ sourceName locationStart
    , showText $ sourceLine locationStart
    , showText $ sourceColumn locationStart
    ]
  toText UnknownLocation = "(unknown)"
  toText GeneratedLocation = "(generated)"
  toText TestLocation = ""
