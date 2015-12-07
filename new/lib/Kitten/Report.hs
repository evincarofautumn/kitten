{-# LANGUAGE OverloadedStrings #-}

module Kitten.Report
  ( Category(..)
  , Item(..)
  , Report(..)
  , reportParseError
  ) where

import Control.Monad.Trans.State (evalState, state)
import Data.Function (on)
import Data.List (nub)
import Kitten.Origin (Origin(Origin))
import qualified Kitten.Origin as Origin
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
import qualified Text.PrettyPrint as Pretty

data Category = Note | Warning | Error | InternalError
  deriving (Eq)

data Item = Item !Origin [Pretty.Doc]
  deriving (Eq)

data Report = Report !Category [Item]
  deriving (Eq)

reportParseError :: Parsec.ParseError -> Report
reportParseError parseError = Report Error
  $ unexpecteds ++ if null expected then [] else [Item origin
    [ "expecting"
    , oxfordOr $ map (Pretty.quotes . Pretty.text) $ nub $ filter (not . null)
      $ map Parsec.messageString expected
    ]]
  where

  origin :: Origin
  origin = Origin
    { Origin.begin = Parsec.errorPos parseError
    , Origin.end = Parsec.errorPos parseError
    }

  sysUnexpected, unexpected, expected :: [Parsec.Message]
  (sysUnexpected, unexpected, expected)
    = flip evalState (Parsec.errorMessages parseError) $ (,,)
      <$> state (span (Parsec.SysUnExpect "" ==))
      <*> state (span (Parsec.UnExpect "" ==))
      <*> state (span (Parsec.Expect "" ==))

  unexpecteds :: [Item]
  unexpecteds = ((++) `on` unexpectedMessages) sysUnexpected unexpected

  unexpectedMessages :: [Parsec.Message] -> [Item]
  unexpectedMessages = nub . map unexpectedMessage

  unexpectedMessage :: Parsec.Message -> Item
  unexpectedMessage message = let
    string = Parsec.messageString message
    in Item origin
      [ "unexpected"
      , if null string then "end of input"
        else Pretty.quotes $ Pretty.text string
      ]

-- FIXME: Figure out an alternative to making Report Show.
instance Show Report where
  show (Report category messages) = unlines $ map showItem $ prefix messages
    where
    prefix (Item origin firstItem : rest)
      = Item origin (categoryPrefix category : firstItem) : rest
    prefix [] = []
    showItem (Item origin message)
      = showOriginPrefix origin ++ Pretty.render (Pretty.hsep message)

showOriginPrefix :: Origin -> String
showOriginPrefix (Origin a b) = concat
  $ [Parsec.sourceName a, ":", show al, ".", show ac, "-"]
  ++ (if al == bl then [show bc] else [show bl, ".", show bc])
  ++ [": "]
  where
  al = Parsec.sourceLine a
  bl = Parsec.sourceLine b
  ac = Parsec.sourceColumn a
  bc = Parsec.sourceColumn b

categoryPrefix :: Category -> Pretty.Doc
categoryPrefix category = case category of
  Note -> "note: "
  Warning -> "warning: "
  Error -> "error: "
  InternalError -> "internal error: "

oxfordOr :: [Pretty.Doc] -> Pretty.Doc
oxfordOr [] = ""
oxfordOr [x] = x
oxfordOr [x, y] = Pretty.hcat [x, "or", y]
oxfordOr [x, y, z] = Pretty.hcat [x, ", ", y, ", or ", z]
oxfordOr (x : xs) = Pretty.hcat [x, ", ", oxfordOr xs]
