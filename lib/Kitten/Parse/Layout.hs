module Kitten.Parse.Layout
  ( insertBraces
  ) where

import Control.Applicative
import Control.Monad

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

  doElse :: Parser [Located]
  doElse = (:) <$> locatedMatch Token.Do <*> doBody

  doBody :: Parser [Located]
  doBody = do
    name <- locatedSatisfy $ \ (Located token _) -> case token of
      Token.Builtin _ -> True
      Token.LittleWord _ -> True
      Token.Operator _ -> True
      _ -> False
    open <- many (unitWhere nonblock)
    body <- unit
    else_ <- option []
      ((:) <$> locatedMatch Token.Else <*> (try doBody <|> unit))
    return $ name : concat [concat open, body, else_]

  unit :: Parser [Located]
  unit = unitWhere $ const True

  unitWhere :: (Located -> Bool) -> Parser [Located]
  unitWhere predicate
    = try (lookAhead $ locatedSatisfy predicate) *> choice
    [ bracket (Token.BlockBegin Token.NormalBlockHint) Token.BlockEnd
    , bracket Token.GroupBegin Token.GroupEnd
    , bracket Token.VectorBegin Token.VectorEnd
    , doElse
    , layout
    , list <$> locatedSatisfy nonbracket
    ]

  nonblock :: Located -> Bool
  nonblock = not . (`elem` blockBrackets) . locatedToken

  nonbracket :: Located -> Bool
  nonbracket = not . (`elem` brackets) . locatedToken

  brackets :: [Token]
  brackets = blockBrackets ++
    [ Token.GroupBegin
    , Token.GroupEnd
    , Token.VectorBegin
    , Token.VectorEnd
    , Token.Else
    ]

  blockBrackets :: [Token]
  blockBrackets =
    [ Token.BlockBegin Token.NormalBlockHint
    , Token.BlockEnd
    , Token.Layout
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
    return $ Located (Token.BlockBegin Token.LayoutBlockHint) colonLoc
      : body ++ [Located Token.BlockEnd colonLoc]
