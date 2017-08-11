{-|
Module      : Kitten.Origin
Description : Source locations
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Origin
  ( HasOrigin(..)
  , Origin
  , begin
  , end
  , merge
  , point
  , pos
  , range
  ) where

import Data.Function (on)
import Data.Text (Text)
import Prelude hiding (span)
import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Text.PrettyPrint as Pretty

-- | A source location, in the form of an origin name (typically a file path)
-- and source span between two ('Line', 'Column') pairs.

data Origin = Origin
  { name :: !Text
  , beginLine :: !Line
  , beginColumn :: !Column
  , endLine :: !Line
  , endColumn :: !Column
  } deriving (Eq, Show)

-- | The starting 'SourcePos' of an 'Origin'.

begin :: Origin -> SourcePos
begin = newPos <$> Text.unpack . name <*> beginLine <*> beginColumn

-- | The ending 'SourcePos' of an 'Origin'.

end :: Origin -> SourcePos
end = newPos <$> Text.unpack . name <*> endLine <*> endColumn

-- | A zero-width 'Origin' at the given 'Line' and 'Column'.

point :: SourceName -> Line -> Column -> Origin
point path line column = Origin
  { name = Text.pack path
  , beginLine = line
  , beginColumn = column
  , endLine = line
  , endColumn = column
  }

-- | Makes a zero-width 'Origin' from a 'SourcePos'.

pos :: SourcePos -> Origin
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

-- | Makes a range between two 'SourcePos' points.
range :: SourcePos -> SourcePos -> Origin
range = merge `on` pos

instance Pretty Origin where
  pPrint origin = Pretty.hcat $
    [ Pretty.text $ Text.unpack $ name origin
    , ":", pPrint al, ".", pPrint ac, "-"
    ]
    ++ (if al == bl then [pPrint bc] else [pPrint bl, ".", pPrint bc])
    where
    al = beginLine origin
    bl = endLine origin
    ac = beginColumn origin
    bc = endColumn origin

class HasOrigin a where
  getOrigin :: a -> Origin

-- | Merges two origins into an origin that includes both. This is essentially a
-- tight bounding box (line). Merging two points produces a range between those
-- points; merging two spans creates a range from the beginning of whichever
-- starts first to the end of whichever ends last.
--
-- When merging origins with different names, I don't believe there's anything
-- sensible to do, nor should it happen in practice, but to avoid throwing an
-- exception, this returns the first argument.

merge :: Origin -> Origin -> Origin
merge a b
  | name a /= name b = a
  | otherwise = Origin
    { name = name a
    , beginLine = minBeginLine
    , beginColumn = minBeginColumn
    , endLine = maxEndLine
    , endColumn = maxEndColumn
    }
  where
    (minBeginLine, minBeginColumn)
      | beginLine a < beginLine b = (beginLine a, beginColumn a)
      | beginLine b < beginLine a = (beginLine b, beginColumn b)
      | otherwise = (beginLine a, (min `on` beginColumn) a b)
    (maxEndLine, maxEndColumn)
      | endLine a < endLine b = (endLine b, endColumn b)
      | endLine b < endLine a = (endLine a, endColumn a)
      | otherwise = (endLine b, (max `on` endColumn) a b)
