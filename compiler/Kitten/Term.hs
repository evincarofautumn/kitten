module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad.Identity
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Data.Vector as Vector
import qualified Text.Parsec as Parsec

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Token (Located(..), Token)
import Kitten.Util

import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !Text !Term
  | Compose !(Vector Term)

data Value
  = Word !Text
  | Int !Int
  | Bool !Bool
  | Text !Text
  | Vec !(Vector Value)
  | Tuple !(Vector Value)
  | Fun !Term

data Element
  = DefElement !(Def Term)
  | TermElement !Term

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
  return $ Fragment (Vector.fromList defs) (compose terms)

element :: Parser Element
element = (DefElement <$> def) <|> (TermElement <$> term)

compose :: [Term] -> Term
compose = Compose . Vector.fromList

def :: Parser (Def Term)
def = (<?> "definition") $ do
  void $ token Token.Def
  mParams <- optionMaybe $ grouped identifier
  name <- identifier
  body <- oneOrBlock
  let
    body' = case mParams of
      Just params -> foldr Lambda body $ reverse params
      Nothing -> body
  return $ Def name body'

term :: Parser Term
term = choice
  [ Value <$> value
  , mapOne toBuiltin <?> "builtin"
  , lambda
  ]
  where
  lambda = (<?> "lambda") $ do
    name <- token Token.Lambda *> identifier
    Lambda name <$> oneOrBlock
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
  vec = Vec . Vector.reverse . Vector.fromList
    <$> between (token Token.VecBegin) (token Token.VecEnd) (many value)
    <?> "vector"
  fun = Fun . compose
    <$> (block term <|> layout)
    <?> "function"
  tuple = Tuple . Vector.reverse . Vector.fromList
    <$> between (token Token.TupleBegin) (token Token.TupleEnd) (many value)
    <?> "tuple"

grouped :: Parser a -> Parser [a]
grouped parser = between (token Token.TupleBegin) (token Token.TupleEnd)
  $ many parser

block :: Parser a -> Parser [a]
block parser = between (token Token.FunBegin) (token Token.FunEnd)
  $ many parser

layout :: Parser [Term]
layout = do
  Token.Located startLocation startIndent _ <- located Token.Layout
  firstToken@(Token.Located firstLocation _ _) <- anyLocated
  let
    inside = if sourceLine firstLocation == sourceLine startLocation
      then \ (Token.Located location _ _)
        -> sourceColumn location > sourceColumn startLocation
      else \ (Token.Located location _ _)
        -> sourceColumn location > startIndent
  innerTokens <- many $ locatedSatisfy inside
  let tokens = firstToken : innerTokens
  case Parsec.parse (many term) "layout quotation" tokens of
    Right result -> return result
    Left err -> fail $ show err

identifier :: Parser Text
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

oneOrBlock :: Parser Term
oneOrBlock = choice
  [ compose <$> block term
  , compose <$> layout
  , term
  ]

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located sourcePos _ _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> justIf (f t) t

mapOne :: (Token -> Maybe a) -> Parser a
mapOne f = tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> f t

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token
token tok = satisfy (== tok)

located :: Token -> Parser Located
located tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
