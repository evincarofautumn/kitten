{-# LANGUAGE RecordWildCards #-}

module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Monoid
import Data.Map (Map)
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno(..), Type((:>), (:.)))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Token (Located(..), Token)
import Kitten.Util.Applicative
import Kitten.Util.Maybe
import Kitten.Util.List

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Push Value
  | Builtin Builtin
  | Lambda String Term
  | Compose [Term]
  deriving (Eq, Show)

data Value
  = Word String
  | Int Int
  | Bool Bool
  | Text String
  | Vec [Value]
  | Tuple [Value]
  | Fun [Term]
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
  return $ Fragment annos defs (Compose terms)

element :: Parser Element
element = choice
  [ AnnoElement <$> anno
  , DefElement <$> def
  , TermElement <$> term
  ]

anno :: Parser Anno
anno = do
  void $ match Token.Type
  params <- maybe mempty makeParamMap
    <$> optionMaybe (grouped $ many identifier)
  name <- identifier
  rawType <- block (sig params) <|> layout (sig params)
  return $ Anno name
    (Set.fromList [Name 0 .. Name $ Map.size params])
    rawType
  where makeParamMap = Map.fromList . flip zip [Name 0 ..]

sig :: Map String Name -> Parser Type
sig params = sig'
  where

  sig' = do
    left <- many1 baseType
    mRight <- optionMaybe $ match Token.Arrow *> many1 baseType
    case mRight of
      Just right -> return $ composeTypes left :> composeTypes right
      Nothing -> return $ composeTypes left

  baseType = choice
    [ Anno.Vec <$> between (match Token.VecBegin) (match Token.VecEnd) sig'
    , Anno.Tuple <$> grouped (many baseType)
    , block sig'
    , var
    ]

  var = do
    word <- identifier
    return $ case Map.lookup word params of
      Just param -> Anno.Var param
      Nothing -> Anno.Word word

  composeTypes = foldl' (:.) Anno.Empty

def :: Parser (Def Term)
def = (<?> "definition") $ do
  void $ match Token.Def
  mParams <- optionMaybe . grouped $ many identifier
  name <- identifier
  body <- oneOrBlock
  let
    body' = case mParams of
      Just params -> foldr Lambda body $ reverse params
      Nothing -> body
  return $ Def name body'

term :: Parser Term
term = choice
  [ Push <$> value
  , mapOne toBuiltin <?> "builtin"
  , lambda
  ]
  where

  lambda = (<?> "lambda") $ do
    name <- match Token.Lambda *> identifier
    Lambda name . Compose <$> many term

  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

value :: Parser Value
value = choice
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

  Token.Located startLocation _ startIndent _ <- located Token.Layout
  firstToken@Token.Located{locatedStart = firstLocation} <- anyLocated

  let
    inside = if sourceLine firstLocation == sourceLine startLocation
      then \ Token.Located{..}
        -> sourceColumn locatedStart > sourceColumn startLocation
      else \ Token.Located{..}
        -> sourceColumn locatedStart > startIndent

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
  closing Located{..} = any (== locatedToken)
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
advance _ _ (Located{..} : _) = locatedStart
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = tokenPrim show advance
  $ \ Located{..} -> justIf (predicate locatedToken) locatedToken

mapOne :: (Token -> Maybe a) -> Parser a
mapOne extract = tokenPrim show advance
  $ \ Located{..} -> extract locatedToken

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ location -> justIf (predicate location) location

match :: Token -> Parser Token
match = satisfy . (==)

located :: Token -> Parser Located
located token = locatedSatisfy $ isLocated token

isLocated :: Token -> Located -> Bool
isLocated token Located{..} = token == locatedToken

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
