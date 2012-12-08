module Term
  ( Term(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad.Identity
import Text.Parsec ((<?>))

import qualified Text.Parsec as P

import Token (Located(..), Token)

import qualified Token as Token

type Parser a = P.ParsecT [Located] () Identity a

data Term
  = Word String
  | Int Integer
  | Def String Term
  | Lambda String Term
  | Vec [Term]
  | Fun [Term]

instance Show Term where
  show (Word word) = word
  show (Int value) = show value
  show (Def name body) = unwords ["def", name, show body]
  show (Lambda name body) = unwords ["\\", name, show body]
  show (Vec terms) = unwords $ "(" : map show terms ++ [")"]
  show (Fun terms) = unwords $ "[" : map show terms ++ ["]"]

parse :: String -> [Located] -> Either P.ParseError [Term]
parse name tokens = P.parse program name tokens

program :: Parser [Term]
program = P.many term <* P.eof

term :: Parser Term
term = P.choice [word, int, def, lambda, vec, fun] <?> "term"
  where
  word = mapOne toWord <?> "word"
  toWord (Token.Word word) = Just (Word word)
  toWord _ = Nothing
  int = mapOne toInt <?> "int"
  toInt (Token.Int value) = Just (Int value)
  toInt _ = Nothing
  def = (<?> "definition") $ do
    (Word name) <- token Token.Def *> word
    body <- term
    return $ Def name body
  lambda = (<?> "lambda") $ do
    (Word name) <- token Token.Lambda *> word
    body <- term
    return $ Lambda name body
  vec = Vec <$> (token Token.VecBegin *> many term <* token Token.VecEnd)
    <?> "vector"
  fun = Fun <$> (token Token.FunBegin *> many term <* token Token.FunEnd)
    <?> "function"

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
