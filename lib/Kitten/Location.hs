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
  | TestLocation

instance Eq Location where
  Location start1 _ == Location start2 _ = start1 == start2
  TestLocation == _ = True
  _ == TestLocation = True
  _ == _ = False

-- Location < UnknownLocation
-- TestLocation = _
instance Ord Location where
  compare a b = case (a, b) of
    _ | a == b -> EQ
    (Location start1 indent1, Location start2 indent2)
      -> compare (start1, indent1) (start2, indent2)
    (TestLocation, _) -> EQ
    (_, TestLocation) -> EQ
    (UnknownLocation, _) -> GT
    (_, UnknownLocation) -> LT

instance Show Location where
  show = T.unpack . toText

instance ToText Location where
  toText Location{..} = T.intercalate ":"
    [ T.pack $ sourceName locationStart
    , showText $ sourceLine locationStart
    , showText $ sourceColumn locationStart
    ]
  toText UnknownLocation = "(unknown)"
  toText TestLocation = ""
