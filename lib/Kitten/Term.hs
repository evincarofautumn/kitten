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
import Kitten.Util.List
import Kitten.Util.Maybe

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
  = Bool Bool Location
  | Escape String Location
  | Float Double Location
  | Function Anno [Term] Location
  | Int Int Location
  | Text String Location
  | Vector (Maybe Anno) [Value] Location
  | Word String Location
  deriving (Eq, Show)

data Element
  = DefElement (Def Term)
  | TermElement Term

parse
  :: String
  -> [Located]
  -> Either ParseError (Fragment Term)
parse name
  = Parsec.parse insertBraces name
  >=> Parsec.parse fragment name

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
    , Anno.Float <$ match Token.FloatType
    , Anno.Int <$ match Token.IntType
    , Anno.Any <$ identifier
    ]

def :: Parser (Def Term)
def = (<?> "definition") . locate
  $ match Token.Def *> (Def <$> identifier <*> term)

insertBraces :: Parser [Located]
insertBraces = (concat <$> many unit) <* eof
  where

  bracket :: Token -> Token -> Parser [Located]
  bracket open close = do
    begin <- locatedMatch open
    inner <- concat <$> many unit
    end <- locatedMatch close
    return $ begin : inner ++ [end]

  ifElse :: Parser [Located]
  ifElse = do
    if_ <- locatedMatch Token.If
    preElse <- concat <$> many unit
    else_ <- locatedMatch Token.Else
    postElse <- unit
    return $ if_ : preElse ++ else_ : postElse

  unit :: Parser [Located]
  unit = unitWhere $ const True

  unitWhere :: (Located -> Bool) -> Parser [Located]
  unitWhere predicate
    = try (lookAhead $ locatedSatisfy predicate) *> choice
    [ bracket Token.BlockBegin Token.BlockEnd
    , bracket Token.GroupBegin Token.GroupEnd
    , bracket Token.VectorBegin Token.VectorEnd
    , ifElse
    , layout
    , list <$> locatedSatisfy (not . isBracket . locatedToken)
    ]

  isBracket :: Token -> Bool
  isBracket token = any (== token)
    [ Token.BlockBegin
    , Token.BlockEnd
    , Token.GroupBegin
    , Token.GroupEnd
    , Token.Layout
    , Token.VectorBegin
    , Token.VectorEnd
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

term :: Parser Term
term = nonblock <|> Block <$> block
  where

  nonblock :: Parser Term
  nonblock = locate $ choice
    [ Push <$> value
    , mapOne toBuiltin
    , lambda
    , if_
    ]

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Lambda *>
    (Lambda <$> identifier <*> many term)

  if_ :: Parser (Location -> Term)
  if_ = If
    <$> (match Token.If *> many nonblock)
    <*> block
    <*> (match Token.Else *> block)

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

block :: Parser [Term]
block = between
  (match Token.BlockBegin) (match Token.BlockEnd)
  (many term) <?> "block"

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
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Float x) = Just $ Float x
  toLiteral (Token.Int x) = Just $ Int x
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

identifier :: Parser String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located _ Location{..} : _) = locationStart
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = tokenPrim show advance $ \ Located{..}
  -> justIf (predicate locatedToken) locatedToken

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ located -> justIf (predicate located) located

mapOne :: (Token -> Maybe a) -> Parser a
mapOne extract = tokenPrim show advance
  $ \ (Located locatedToken _) -> extract locatedToken

match :: Token -> Parser Token
match = satisfy . (==)

locatedMatch :: Token -> Parser Located
locatedMatch token
  = locatedSatisfy ((token ==) . locatedToken)

locate :: Parser (Location -> a) -> Parser a
locate parser = do
  start <- getPosition
  result <- parser
  return $ result Location
    { locationStart = start
    , locationIndent = -1  -- FIXME
    }
