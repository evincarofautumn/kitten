{-# LANGUAGE RecordWildCards #-}

module Kitten.Location
  ( Location(..)
  ) where

import Text.Parsec.Pos

data Location
  = Location
    { locationStart :: SourcePos
    , locationEnd :: SourcePos
    , locationIndent :: Column
    }
  | DerivedLocation
  | TestLocation

instance Eq Location where
  Location start1 end1 indent1
    == Location start2 end2 indent2
    = (start1, end1, indent1) == (start2, end2, indent2)
  TestLocation == _ = True
  _ == TestLocation = True
  _ == _ = False

instance Show Location where
  show Location{..} = concat
    [ show locationStart
    , "-"
    , show locationEnd
    ]
  show DerivedLocation = "derived"  -- FIXME
  show TestLocation = ""
