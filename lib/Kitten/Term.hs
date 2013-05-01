{-# LANGUAGE RecordWildCards #-}

module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno(..), Type((:>)))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Location
import Kitten.Token (Located(..), Token)
import Kitten.Util.Applicative
import Kitten.Util.Maybe
import Kitten.Util.List

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Push Value Location
  | Builtin Builtin Location
  | Lambda String [Term] Location
  | Block [Term]
  | If [Term] [Term] [Term] Location
  deriving (Eq, Show)

data Value
  = Word String Location
  | Int Int Location
  | Bool Bool Location
  | Text String Location
  | Vector (Maybe Anno) [Value] Location
  | Function Anno [Term] Location
  | Escape String Location
  deriving (Eq, Show)

data Element
  = DefElement (Def Term)
  | TermElement Term

parse :: String -> [Located] -> Either ParseError (Fragment Term)
parse = Parsec.parse fragment

partitionElements :: [Element] -> ([Def Term], [Term])
partitionElements = foldr partitionElement mempty
  where
  partitionElement (DefElement d) (ds, ts) = (d : ds, ts)
  partitionElement (TermElement t) (ds, ts) = (ds, t : ts)

fragment :: Parser (Fragment Term)
fragment = do
  elements <- many element <* eof
  let (defs, terms) = partitionElements elements
  return $ Fragment defs terms

element :: Parser Element
element = choice
  [ DefElement <$> def
  , TermElement <$> term
  ]

signature :: Parser Anno
signature = locate $ Anno <$> signature'
  where

  signature' = do
    left <- many baseType
    mRight <- optionMaybe $ match Token.Arrow *> many baseType
    return $ case mRight of
      Just right
        -> Anno.Composition left
        :> Anno.Composition right
      Nothing
        -> Anno.Composition []
        :> Anno.Composition left

  baseType = (<?> "base type") $ choice
    [ Anno.Vector <$> between
      (match Token.VectorBegin)
      (match Token.VectorEnd)
      signature'
    , grouped signature'
    , Anno.Bool <$ match Token.BoolType
    , Anno.Text <$ match Token.TextType
    , Anno.Int <$ match Token.IntType
    , Anno.Any <$ identifier  -- FIXME
    ]

def :: Parser (Def Term)
def = (<?> "definition") . locate
  $ match Token.Def *> (Def <$> identifier <*> term)

term :: Parser Term
term = choice
  [ locate $ Push <$> value
  , locate (mapOne toBuiltin <?> "builtin")
  , locate lambda
  , locate if_
  , Block <$> block
  ]
  where

  lambda = (<?> "lambda") $ match Token.Lambda *>
    (Lambda <$> identifier <*> many term)

  if_ = If
    <$> (match Token.If *> many term)
    <*> (match Token.Then *> block)
    <*> (match Token.Else *> block)

  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

block :: Parser [Term]
block = blocked (many term) <|> layout (many term)
  <?> "block"

value :: Parser Value
value = locate $ choice
  [ mapOne toLiteral <?> "literal"
  , mapOne toWord <?> "word"
  , annotated
  , escape
  , Vector Nothing <$> vector
  ]
  where

  toLiteral :: Token -> Maybe (Location -> Value)
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Text x) = Just $ Text x
  toLiteral _ = Nothing

  toWord :: Token -> Maybe (Location -> Value)
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing

  annotated :: Parser (Location -> Value)
  annotated = do
    anno <- grouped signature
    (Vector (Just anno) <$> vector)
      <|> (Function anno <$> block)

  escape :: Parser (Location -> Value)
  escape = Escape <$> (match Token.Escape *> identifier)

  vector :: Parser [Value]
  vector = (reverse <$> between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (many value))
    <?> "vector"

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)

blocked :: Parser a -> Parser a
blocked = between
  (match Token.BlockBegin)
  (match Token.BlockEnd)

locatedBetween
  :: Token
  -> Token
  -> (Located -> Bool)
  -> Parser [Located]
  -> Parser [Located]
locatedBetween begin end inside parser = do
  open <- locatedSatisfy $ inside .&&. isLocated begin
  body <- parser
  close <- located end
  return $ open : body ++ [close]

locatedBlock
  :: (Located -> Bool)
  -> Parser [Located]
  -> Parser [Located]
locatedBlock = locatedBetween Token.BlockBegin Token.BlockEnd

locatedGroup
  :: (Located -> Bool)
  -> Parser [Located]
  -> Parser [Located]
locatedGroup = locatedBetween Token.GroupBegin Token.GroupEnd

locatedVector
  :: (Located -> Bool)
  -> Parser [Located]
  -> Parser [Located]
locatedVector = locatedBetween Token.VectorBegin Token.VectorEnd

layout :: Parser a -> Parser a
layout inner = do

  Token.Located _ Location
    { locationStart = startLocation
    , locationIndent = startIndent
    } <- located Token.Layout
  Token.Located _ Location{locationStart = firstLocation}
    <- lookAhead anyLocated

  let
    inside = if sourceLine firstLocation == sourceLine startLocation
      then \ (Token.Located _ Location{..})
        -> sourceColumn locationStart > sourceColumn startLocation
      else \ (Token.Located _ Location{..})
        -> sourceColumn locationStart > startIndent

    innerTokens = concat <$> many innerToken

    innerToken = choice
      [ locatedBlock inside innerTokens
      , locatedVector inside innerTokens
      , locatedGroup inside innerTokens
      , liftM list . locatedSatisfy $ inside .&&. not . bracket
      ]

  tokens <- innerTokens
  when (null tokens)
    $ fail "empty layout blocks are not allowed; use {} instead"

  case Parsec.parse (inner <* eof) "layout" tokens of
    Right result -> return result
    Left err -> fail $ show err
  where

  bracket :: Located -> Bool
  bracket (Located token _) = any (== token)
    [ Token.BlockBegin
    , Token.BlockEnd
    , Token.GroupBegin
    , Token.GroupEnd
    , Token.VectorBegin
    , Token.VectorEnd
    ]

identifier :: Parser String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located _ Location{..} : _) = locationStart
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = tokenPrim show advance
  $ \ (Located locatedToken _)
  -> justIf (predicate locatedToken) locatedToken

mapOne :: (Token -> Maybe a) -> Parser a
mapOne extract = tokenPrim show advance
  $ \ (Located locatedToken _) -> extract locatedToken

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ location -> justIf (predicate location) location

match :: Token -> Parser Token
match = satisfy . (==)

locate :: Parser (Location -> a) -> Parser a
locate parser = do
  start <- getPosition
  result <- parser
  return $ result Location
    { locationStart = start
    , locationIndent = 0  -- FIXME
    }

located :: Token -> Parser Located
located token = locatedSatisfy $ isLocated token

isLocated :: Token -> Located -> Bool
isLocated token (Located locatedToken _)
  = token == locatedToken

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
