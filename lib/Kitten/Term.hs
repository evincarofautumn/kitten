{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad
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

type Parser m a = ParsecT [Located] () m a

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

parse
  :: (Monad m)
  => String
  -> [Located]
  -> m (Either ParseError (Fragment Term))
parse name tokens = Parsec.runParserT fragment () name tokens

partitionElements :: [Element] -> ([Def Term], [Term])
partitionElements = foldr partitionElement mempty
  where
  partitionElement (DefElement d) (ds, ts) = (d : ds, ts)
  partitionElement (TermElement t) (ds, ts) = (ds, t : ts)

fragment :: (Monad m) => Parser m (Fragment Term)
fragment = do
  elements <- many element <* eof
  let (defs, terms) = partitionElements elements
  return $ Fragment defs terms

element :: (Monad m) => Parser m Element
element = choice
  [ DefElement <$> def
  , TermElement <$> term
  ]

signature :: (Monad m) => Parser m Anno
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
    , Anno.Any <$ identifier
    ]

def :: (Monad m) => Parser m (Def Term)
def = (<?> "definition") . locate
  $ match Token.Def *> (Def <$> identifier <*> term)

term :: (Monad m) => Parser m Term
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
    <*> (match Token.Then *> (block <|> list <$> term))
    <*> (match Token.Else *> (block <|> list <$> term))

  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

block :: (Monad m) => Parser m [Term]
block = blocked (many term) <|> layout (many term)
  <?> "block"

value :: forall m. (Monad m) => Parser m Value
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

  annotated :: Parser m (Location -> Value)
  annotated = do
    anno <- grouped signature
    (Vector (Just anno) <$> vector)
      <|> (Function anno <$> block)

  escape :: Parser m (Location -> Value)
  escape = Escape <$> (match Token.Escape *> identifier)

  vector :: Parser m [Value]
  vector = (reverse <$> between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (many value))
    <?> "vector"

grouped :: (Monad m) => Parser m a -> Parser m a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)

blocked :: (Monad m) => Parser m a -> Parser m a
blocked = between
  (match Token.BlockBegin)
  (match Token.BlockEnd)

locatedBetween
  :: (Monad m)
  => Token
  -> Token
  -> (Located -> Bool)
  -> Parser m [Located]
  -> Parser m [Located]
locatedBetween begin end inside parser = do
  open <- locatedSatisfy $ inside .&&. isLocated begin
  body <- parser
  close <- located end
  return $ open : body ++ [close]

locatedBlock
  :: (Monad m)
  => (Located -> Bool)
  -> Parser m [Located]
  -> Parser m [Located]
locatedBlock = locatedBetween Token.BlockBegin Token.BlockEnd

locatedGroup
  :: (Monad m)
  => (Located -> Bool)
  -> Parser m [Located]
  -> Parser m [Located]
locatedGroup = locatedBetween Token.GroupBegin Token.GroupEnd

locatedVector
  :: (Monad m)
  => (Located -> Bool)
  -> Parser m [Located]
  -> Parser m [Located]
locatedVector = locatedBetween Token.VectorBegin Token.VectorEnd

layout :: (Monad m) => (forall n. (Monad n) => Parser n a) -> Parser m a
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

  position <- getPosition
  parsed <- parseLocal (inner <* eof) position tokens
  case parsed of
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

parseLocal
  :: (Monad m)
  => Parser m a
  -> SourcePos
  -> [Located]
  -> m (Either ParseError a)
parseLocal parser position input = do
  parsed <- runParsecT parser (State input position ())
  reply <- parserReply parsed
  case reply of
    Ok result _ _ -> return $ Right result
    Error parseError -> return $ Left parseError

  where
  parserReply res = case res of
    Parsec.Consumed r -> r
    Parsec.Empty r -> r

identifier :: (Monad m) => Parser m String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located _ Location{..} : _) = locationStart
advance sourcePos _ _ = sourcePos

satisfy :: (Monad m) => (Token -> Bool) -> Parser m Token
satisfy predicate = tokenPrim show advance
  $ \ (Located locatedToken _)
  -> justIf (predicate locatedToken) locatedToken

mapOne :: (Monad m) => (Token -> Maybe a) -> Parser m a
mapOne extract = tokenPrim show advance
  $ \ (Located locatedToken _) -> extract locatedToken

locatedSatisfy :: (Monad m) => (Located -> Bool) -> Parser m Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ location -> justIf (predicate location) location

match :: (Monad m) => Token -> Parser m Token
match = satisfy . (==)

locate :: (Monad m) => Parser m (Location -> a) -> Parser m a
locate parser = do
  start <- getPosition
  result <- parser
  return $ result Location
    { locationStart = start
    , locationIndent = 0  -- FIXME
    }

located :: (Monad m) => Token -> Parser m Located
located token = locatedSatisfy $ isLocated token

isLocated :: Token -> Located -> Bool
isLocated token (Located locatedToken _)
  = token == locatedToken

anyLocated :: (Monad m) => Parser m Located
anyLocated = locatedSatisfy (const True)
