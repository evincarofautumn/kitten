module Kitten.Abbreviation
  ( Abbreviation(..)
  ) where

import Data.Text (Text)

import Kitten.Location
import Kitten.Name

data Abbreviation = Abbreviation !Qualifier !Text !Qualifier !Location
  deriving (Eq, Show)

