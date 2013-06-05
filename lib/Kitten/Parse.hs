module Kitten.Parse
  ( parse
  ) where

import Control.Applicative
import Control.Monad

import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno(..))
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

import qualified Kitten.Anno as Anno
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
  let (defs, terms) = partitionElements elements
  return $ Fragment defs terms

element :: Parser Element
element = choice
  [ DefElement <$> def
  , TermElement <$> term
  ]

def :: Parser (Def Value)
def = (<?> "definition") . locate
  $ match Token.Def *> (Def <$> littleWord <*> value)

term :: Parser Term
term = nonblock <|> Block <$> block
  where

  nonblock :: Parser Term
  nonblock = locate $ choice
    [ Push <$> value
    , Call <$> littleWord
    , mapOne toBuiltin
    , lambda
    , if_
    ]

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Arrow *>
    (Lambda <$> littleWord <*> many term)

  if_ :: Parser (Location -> Term)
  if_ = If
    <$> (match Token.If *> many nonblock)
    <*> block
    <*> (match Token.Else *> (block <|> list <$> locate if_))

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

value :: Parser Value
value = locate $ choice
  [ mapOne toLiteral <?> "literal"
  , try annotated
  , pair <$> tuple
  , escape
  , Vector Nothing <$> vector
  ]
  where

  toLiteral :: Token -> Maybe (Location -> Value)
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Char x) = Just $ Char x
  toLiteral (Token.Float x) = Just $ Float x
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Text x) = Just $ \ loc
    -> Vector (Just $ Anno Anno.Char loc)
      (map (`Char` loc) $ reverse x) loc
  toLiteral _ = Nothing

  annotated :: Parser (Location -> Value)
  annotated = do
    anno <- grouped signature
    (Vector (Just anno) <$> vector)
      <|> (Function (Just anno) <$> block)

  escape :: Parser (Location -> Value)
  escape = Escape <$> (match Token.Escape *> littleWord)

  vector :: Parser [Value]
  vector = (reverse <$> between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (value `sepEndBy` match Token.Comma))
    <?> "vector"

  tuple :: Parser [Value]
  tuple = grouped (value `sepEndBy` match Token.Comma)
    <?> "tuple"

  pair values loc
    = foldr (\ x y -> Pair x y loc) (Unit loc) values

block :: Parser [Term]
block = blocked (many term) <?> "block"
