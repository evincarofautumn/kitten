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
import Data.Map (Map)
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno(..), Type((:>)))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Kind
import Kitten.Location
import Kitten.Name
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
  | Lambda String Term
  | Compose [Term]
  deriving (Eq, Show)

data Value
  = Word String Location
  | Int Int Location
  | Bool Bool Location
  | Text String Location
  | Vec [Value] Location
  | Tuple [Value] Location
  | Fun [Term] Location
  deriving (Eq, Show)

data Element
  = AnnoElement Anno
  | DefElement (Def Term)
  | TermElement Term

parse :: String -> [Located] -> Either ParseError (Fragment Term)
parse = Parsec.parse fragment

partitionElements :: [Element] -> ([Anno], [Def Term], [Term])
partitionElements = foldr partitionElement mempty
  where
  partitionElement (AnnoElement a) (as, ds, ts) = (a : as, ds, ts)
  partitionElement (DefElement d) (as, ds, ts) = (as, d : ds, ts)
  partitionElement (TermElement t) (as, ds, ts) = (as, ds, t : ts)

fragment :: Parser (Fragment Term)
fragment = do
  elements <- many element <* eof
  let (annos, defs, terms) = partitionElements elements
  return $ Fragment annos defs terms

element :: Parser Element
element = choice
  [ AnnoElement <$> anno
  , DefElement <$> def
  , TermElement <$> term
  ]

anno :: Parser Anno
anno = locate $ do
  void $ match Token.Type
  params <- maybe mempty makeParamMap
    <$> optionMaybe (grouped $ many identifier)
  name <- identifier
  rawType <- block (sig params) <|> layout (sig params)
  return $ Anno name
    (Set.fromList [Name 0 .. Name . pred $ Map.size params])
    rawType
  where makeParamMap = Map.fromList . flip zip [Name 0 ..]

sig :: Map String Name -> Parser (Type Scalar)
sig params = sig'
  where

  sig' = do
    left <- many baseType
    mRight <- optionMaybe $ match Token.Arrow *> many baseType
    case mRight of
      Just right -> return
        $ Anno.Composition (reverse left)
        :> Anno.Composition (reverse right)
      Nothing -> return
        $ Anno.Composition []
        :> Anno.Composition (reverse left)

  baseType = (<?> "base type") $ choice
    [ Anno.Vec <$> between
      (match Token.VecBegin)
      (match Token.VecEnd)
      sig'
    , Anno.Tuple <$> grouped (many baseType)
    , block sig'
    , Anno.Bool <$ match Token.BoolType
    , Anno.Text <$ match Token.TextType
    , Anno.Int <$ match Token.IntType
    , var
    ]

  var = (<?> "builtin type or type variable") $ do
    word <- identifier
    case Map.lookup word params of
      Just param -> return $ Anno.Var param
      Nothing -> unexpected word

def :: Parser (Def Term)
def = (<?> "definition") . locate $ do
  void $ match Token.Def
  mParams <- optionMaybe . grouped $ many identifier
  name <- identifier
  body <- oneOrBlock
  let
    body' = case mParams of
      Just params -> foldr Lambda body (reverse params)
      Nothing -> body
  return $ Def name body'

term :: Parser Term
term = choice
  [ locate $ Push <$> value
  , locate (mapOne toBuiltin <?> "builtin")
  , lambda
  ]
  where

  lambda = (<?> "lambda") $ do
    name <- match Token.Lambda *> identifier
    terms <- many term
    return $ Lambda name (Compose terms)

  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

value :: Parser Value
value = locate $ choice
  [ mapOne toLiteral <?> "literal"
  , mapOne toWord <?> "word"
  , vec
  , fun
  , tuple
  ]
  where

  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Text x) = Just $ Text x
  toLiteral _ = Nothing

  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing

  vec = Vec . reverse
    <$> between
      (match Token.VecBegin)
      (match Token.VecEnd)
      (many value)
    <?> "vector"

  fun = Fun
    <$> (block (many term) <|> layout (many term))
    <?> "function"

  tuple = Tuple . reverse
    <$> between
      (match Token.TupleBegin)
      (match Token.TupleEnd)
      (many value)
    <?> "tuple"

grouped :: Parser a -> Parser a
grouped = between (match Token.TupleBegin) (match Token.TupleEnd)

block :: Parser a -> Parser a
block = between (match Token.FunBegin) (match Token.FunEnd)

locatedBlock :: (Located -> Bool) -> Parser a -> Parser a
locatedBlock inside = between
  (locatedSatisfy $ inside .&&. isLocated Token.FunBegin)
  (match Token.FunEnd)

locatedTuple :: (Located -> Bool) -> Parser a -> Parser a
locatedTuple inside = between
  (locatedSatisfy $ inside .&&. isLocated Token.TupleBegin)
  (match Token.TupleEnd)

locatedVec :: (Located -> Bool) -> Parser a -> Parser a
locatedVec inside = between
  (locatedSatisfy $ inside .&&. isLocated Token.VecBegin)
  (match Token.VecEnd)

layout :: Parser a -> Parser a
layout inner = do

  Token.Located _ Location
    { locationStart = startLocation
    , locationIndent = startIndent
    } <- located Token.Layout
  firstToken
    @(Token.Located _ Location{locationStart = firstLocation})
    <- anyLocated

  let
    inside = if sourceLine firstLocation == sourceLine startLocation
      then \ (Token.Located _ Location{..})
        -> sourceColumn locationStart > sourceColumn startLocation
      else \ (Token.Located _ Location{..})
        -> sourceColumn locationStart > startIndent

  innerTokens <- liftM concat . many $ choice
    [ locatedBlock inside $ many anyLocated
    , locatedVec inside $ many anyLocated
    , locatedTuple inside $ many anyLocated
    , liftM list . locatedSatisfy $ inside .&&. not . closing
    ]

  let tokens = firstToken : innerTokens
  case Parsec.parse (inner <* eof) "layout" tokens of
    Right result -> return result
    Left err -> fail $ show err
  where

  closing :: Located -> Bool
  closing (Located token _) = any (== token)
    [ Token.FunEnd
    , Token.VecEnd
    , Token.TupleEnd
    ]

identifier :: Parser String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

oneOrBlock :: Parser Term
oneOrBlock = choice
  [ Compose <$> block (many term)
  , Compose <$> layout (many term)
  , term
  ]

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
  end <- getPosition
  return $ result Location
    { locationStart = start
    , locationEnd = end
    , locationIndent = 0  -- FIXME
    }

located :: Token -> Parser Located
located token = locatedSatisfy $ isLocated token

isLocated :: Token -> Located -> Bool
isLocated token (Located locatedToken _)
  = token == locatedToken

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
