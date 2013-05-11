{-# LANGUAGE RecordWildCards #-}

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
  -> Either ParseError (Fragment Term)
parse name
  = Parsec.parse insertBraces name
  >=> Parsec.parse fragment name

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

def :: Parser (Def Term)
def = (<?> "definition") . locate
  $ match Token.Def *> (Def <$> littleWord <*> term)

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
  , Word <$> littleWord <?> "word"
  , annotated
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
      (map (\ c -> Char c loc) x) loc
  toLiteral _ = Nothing

  annotated :: Parser (Location -> Value)
  annotated = do
    anno <- grouped signature
    (Vector (Just anno) <$> vector)
      <|> (Function anno <$> block)

  escape :: Parser (Location -> Value)
  escape = Escape <$> (match Token.Escape *> littleWord)

  vector :: Parser [Value]
  vector = (reverse <$> between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (many value))
    <?> "vector"

block :: Parser [Term]
block = blocked (many term) <?> "block"
