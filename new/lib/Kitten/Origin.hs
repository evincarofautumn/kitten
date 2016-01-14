module Kitten.Origin
  ( Origin(..)
  , point
  ) where

import Text.Parsec.Pos

data Origin = Origin { begin :: !SourcePos, end :: !SourcePos }
  deriving (Eq, Show)

point :: SourceName -> Line -> Column -> Origin
point name line column = let
  pos = newPos name line column
  in Origin { begin = pos, end = pos }
