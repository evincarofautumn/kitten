module Term
  ( Def(..)
  , Program(..)
  , Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Data.Either
import Data.List
import Text.Parsec ((<?>))

import qualified Text.Parsec as P

import Builtin (Builtin)
import Def
import Program
import Token (Located(..), Token)
import Util

import qualified Token

type Parser a = P.ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !String !Term
  | Compose !Term !Term
  | Empty

data Value
  = Word !String
  | Int !Int
  | Bool !Bool
  | Vec ![Value]
  | Fun !Term

parse :: String -> [Located] -> Either P.ParseError (Program Term)
parse = P.parse program

program :: Parser (Program Term)
program = uncurry Program . second compose . partitionEithers
  <$> P.many ((Left <$> def) <|> (Right <$> term)) <* P.eof

compose :: [Term] -> Term
compose = foldl' Compose Empty

def :: Parser (Def Term)
def = (<?> "definition") $ do
  name <- token Token.Def *> identifier
  Def name <$> grouped

term :: Parser Term
term = P.choice [Value <$> value, builtin, lambda]

value :: Parser Value
value = P.choice
  [ literal
  , vec
  , fun
  , word
  ]
  where
  literal = mapOne toLiteral <?> "literal"
  toLiteral (Token.Int value) = Just $ Int value
  toLiteral (Token.Bool value) = Just $ Bool value
  toLiteral _ = Nothing
  vec = Vec <$> (token Token.VecBegin *> many value <* token Token.VecEnd)
    <?> "vector"
  fun = Fun . compose
    <$> (token Token.FunBegin *> many term <* token Token.FunEnd)
    <?> "function"
  word = mapOne toWord <?> "word"
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing

identifier :: Parser String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

builtin :: Parser Term
builtin = mapOne toBuiltin <?> "builtin"
  where
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

lambda = (<?> "lambda") $ do
  name <- token Token.Lambda *> identifier
  Lambda name <$> grouped

grouped :: Parser Term
grouped
  = compose <$> (token Token.FunBegin *> many term <* token Token.FunEnd)
  <|> term

advance :: P.SourcePos -> t -> [Located] -> P.SourcePos
advance _ _ (Located sourcePos _ _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = P.tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> justIf (f t) t

mapOne :: (Token -> Maybe a) -> Parser a
mapOne f = P.tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> f t

justIf :: Bool -> a -> Maybe a
justIf c x = if c then Just x else Nothing

locatedSatisfy
  :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = P.tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token -- P.ParsecT s u m Token
token tok = satisfy (== tok)

locatedToken :: Token -> Parser Located
locatedToken tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocatedToken :: Parser Located
anyLocatedToken = locatedSatisfy (const True)
