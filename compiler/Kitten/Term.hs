module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad.Identity
import Data.List
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

import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !Text !Term
  | Compose !Term !Term
  | Empty

data Value
  = Word !Text
  | Int !Int
  | Bool !Bool
  | Text !Text
  | Vec !(Vector Value)
  | Fun !Term

data Element
  = DefElement (Def Term)
  | TermElement Term

parse :: String -> [Located] -> Either ParseError (Fragment Term)
parse = Parsec.parse fragment

partitionElements :: [Element] -> ([Def Term], [Term])
partitionElements = partitionElements' mempty
  where
  partitionElements' (ds, ts) (DefElement d : es)
    = partitionElements' (d : ds, ts) es
  partitionElements' (ds, ts) (TermElement t : es)
    = partitionElements' (ds, t : ts) es
  partitionElements' acc [] = acc

fragment :: Parser (Fragment Term)
fragment = do
  elements <- many element <* eof
  let (defs, terms) = partitionElements elements
  return $ Fragment (Vector.fromList defs) (compose terms)

element :: Parser Element
element = choice
  [ DefElement <$> def
  , TermElement <$> term
  ]

compose :: [Term] -> Term
compose = foldl' Compose Empty

def :: Parser (Def Term)
def = (<?> "definition") $ do
  void $ token Token.Def
  mParams <- optionMaybe $ grouped identifier
  name <- identifier
  body <- oneOrGroup
  let
    body' = case mParams of
      Just params -> foldr Lambda body $ reverse params
      Nothing -> body
  return $ Def name body'

term :: Parser Term
term = choice [Value <$> value, builtin, lambda]

value :: Parser Value
value = choice
  [ literal
  , vec
  , fun
  , word
  ]
  where
  literal = mapOne toLiteral <?> "literal"
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Text x) = Just $ Text x
  toLiteral _ = Nothing
  vec = Vec . Vector.fromList
    <$> (token Token.VecBegin *> many value <* token Token.VecEnd)
    <?> "vector"
  word = mapOne toWord <?> "word"
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing
  fun = Fun . compose
    <$> (grouped term <|> layout)
    <?> "function"

grouped :: Parser a -> Parser [a]
grouped parser = token Token.FunBegin *> many parser <* token Token.FunEnd

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

builtin :: Parser Term
builtin = mapOne toBuiltin <?> "builtin"
  where
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

lambda :: Parser Term
lambda = (<?> "lambda") $ do
  name <- token Token.Lambda *> identifier
  Lambda name <$> oneOrGroup

oneOrGroup :: Parser Term
oneOrGroup = choice
  [ compose <$> grouped term
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

justIf :: Bool -> a -> Maybe a
justIf c x = if c then Just x else Nothing

locatedSatisfy
  :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token
token tok = satisfy (== tok)

located :: Token -> Parser Located
located tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
