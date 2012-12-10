module Term
  ( Def(..)
  , Program(..)
  , Term(..)
  , parse
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Data.Either
import Data.List
import Text.Parsec ((<?>))

import qualified Text.Parsec as P

import Def
import Program
import Token (Located(..), Token)

import qualified Token

type Parser a = P.ParsecT [Located] () Identity a

data Term
  = Word !String
  | Int !Integer
  | Lambda !String !Term
  | Vec ![Term]
  | Fun !Term
  | Compose !Term !Term
  | Empty

instance Show Term where
  show (Word name) = name
  show (Int value) = show value
  show (Lambda name body) = unwords ["\\", name, show body]
  show (Vec body) = "(" ++ unwords (map show body) ++ ")"
  show (Fun body) = "[" ++ show body ++ "]"
  show (Compose down top) = show down ++ ' ' : show top
  show Empty = ""

parse :: String -> [Located] -> Either P.ParseError (Program Term)
parse = P.parse program

program :: Parser (Program Term)
program = uncurry Program . second compose . partitionEithers
  <$> P.many ((Left <$> def) <|> (Right <$> term)) <* P.eof

compose :: [Term] -> Term
compose = foldl' Compose Empty

def :: Parser (Def Term)
def = (<?> "definition") $ do
  Word name <- token Token.Def *> word
  body <- term
  return $ Def name body

term :: Parser Term
term = P.choice [word, int, lambda, vec, fun] <?> "term"
  where
  int = mapOne toInt <?> "int"
  toInt (Token.Int value) = Just (Int value)
  toInt _ = Nothing
  lambda = (<?> "lambda") $ do
    Word name <- token Token.Lambda *> word
    body <- term
    return $ Lambda name body
  vec = Vec <$> (token Token.VecBegin *> many term <* token Token.VecEnd)
    <?> "vector"
  fun = Fun . compose
    <$> (token Token.FunBegin *> many term <* token Token.FunEnd)
    <?> "function"

word :: Parser Term
word = mapOne toWord <?> "word"
  where
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing

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
