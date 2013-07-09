module Kitten.Parse.Layout
  ( insertBraces
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Kitten.Location
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Token (Located(..), Token)
import Kitten.Util.List

import qualified Kitten.Token as Token

insertBraces :: Parser [Located]
insertBraces = (concat <$> many unit) <* eof
  where

  bracket :: Token -> Token -> Parser [Located]
  bracket open close = do
    begin <- locatedMatch open
    inner <- concat <$> many unit
    end <- locatedMatch close
    return $ begin : inner ++ [end]

  ifThenElse :: Parser [Located]
  ifThenElse = do
    if_ <- (:) <$> locatedMatch Token.If <*> units
    then_ <- (:) <$> locatedMatch Token.Then <*> unit
    else_ <- fromMaybe [] <$> optionMaybe
      ((:) <$> locatedMatch Token.Else <*> unit)
    return $ concat [if_, then_, else_]

  unit :: Parser [Located]
  unit = unitWhere $ const True

  units :: Parser [Located]
  units = concat <$> many unit

  unitWhere :: (Located -> Bool) -> Parser [Located]
  unitWhere predicate
    = try (lookAhead $ locatedSatisfy predicate) *> choice
    [ bracket Token.BlockBegin Token.BlockEnd
    , bracket Token.GroupBegin Token.GroupEnd
    , bracket Token.VectorBegin Token.VectorEnd
    , ifThenElse
    , layout
    , list <$> locatedSatisfy (not . isBracket . locatedToken)
    ]

  isBracket :: Token -> Bool
  isBracket token = token `elem`
    [ Token.BlockBegin
    , Token.BlockEnd
    , Token.GroupBegin
    , Token.GroupEnd
    , Token.Layout
    , Token.VectorBegin
    , Token.VectorEnd
    , Token.Then
    , Token.Else
    ]

  layout :: Parser [Located]
  layout = do
    Located
      { locatedLocation = colonLoc@Location
        {locationIndent = colonIndent}
      } <- locatedMatch Token.Layout
    let
      inside (Located _ loc)
        = sourceColumn (locationStart loc) > colonIndent
    body <- concat <$> many (unitWhere inside)
    when (null body)
      $ fail "empty layout blocks are not allowed; use {} instead"
    return $ Located Token.BlockBegin colonLoc
      : body ++ [Located Token.BlockEnd colonLoc]
