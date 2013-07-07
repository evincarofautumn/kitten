module Kitten.Parse
  ( parse
  ) where

import Control.Applicative
import Control.Monad

import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno)
import Kitten.Def
import Kitten.Fragment
import Kitten.Location
import Kitten.Parsec
import Kitten.Parse.Element
import Kitten.Parse.Layout
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parse.Type
import Kitten.Term
import Kitten.Token (Located(..), Token)
import Kitten.Util.List

import qualified Kitten.Token as Token

parse
  :: String
  -> [Located]
  -> Either ParseError (Fragment Value Term)
parse name
  = Parsec.parse insertBraces name
  >=> Parsec.parse fragment name

fragment :: Parser (Fragment Value Term)
fragment = do
  elements <- many element <* eof
  let (decls, defs, terms) = partitionElements elements
  return $ Fragment decls defs terms

element :: Parser Element
element = choice
  [ DeclElement <$> decl
  , DefElement <$> def
  , TermElement <$> term
  ]

decl :: Parser (Def Anno)
decl = (<?> "type declaration") . locate
  $ match Token.Decl
  *> (Def <$> littleWord <*> grouped signature)

def :: Parser (Def Value)
def = (<?> "definition") . locate
  $ match Token.Def
  *> (Def <$> littleWord <*> value)

term :: Parser Term
term = locate $ choice
  [ Push <$> value
  , Call <$> littleWord
  , VectorTerm <$> vector
  , pair <$> tuple
  , mapOne toBuiltin <?> "builtin"
  , lambda
  , if_
  ]
  where

  if_ :: Parser (Location -> Term)
  if_ = If
    <$> (match Token.If *> many term)
    <*> (match Token.Then *> branch)
    <*> (match Token.Else *> branch)
    <?> "if"
    where branch = block <|> list <$> locate if_

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> many term
    , do
      names <- blocked (many littleWord)
      terms <- many term
      return $ \ loc -> foldr
        (\ lambdaName lambdaTerms -> Lambda lambdaName [lambdaTerms] loc)
        (Block terms)
        (reverse names)
    ]

  -- pair :: [Value] -> Location -> Value
  pair values loc
    = foldr (\ x y -> PairTerm x [y] loc) (Push (Unit loc) loc) values

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser [[Term]]
  tuple = grouped (many1 term `sepEndBy1` match Token.Comma)
    <?> "tuple"

  vector :: Parser [[Term]]
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (many1 term `sepEndBy` match Token.Comma)
    <?> "vector"

value :: Parser Value
value = locate $ choice
  [ mapOne toLiteral <?> "literal"
  , Function <$> block <?> "function"
  , try unit
  ]
  where

  toLiteral :: Token -> Maybe (Location -> Value)
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Char x) = Just $ Char x
  toLiteral (Token.Float x) = Just $ Float x
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Text x) = Just $ \ loc
    -> Vector (map (`Char` loc) x) loc
  toLiteral _ = Nothing

  unit :: Parser (Location -> Value)
  unit = Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser [Term]
block = blocked (many term) <?> "block"
