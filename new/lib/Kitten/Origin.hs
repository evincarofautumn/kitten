module Kitten.Origin
  ( Origin(..)
  ) where

import Text.Parsec.Pos (SourcePos)

data Origin = Origin { begin :: !SourcePos, end :: !SourcePos }
  deriving (Eq, Show)
