{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( baseType
  , scalarQuantifier
  , signature
  , typeDefType
  ) where

import Control.Applicative
import Data.Either
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive
import Kitten.Types
import Kitten.Util.Parsec
import Kitten.Util.Tuple

signature :: Parser Anno
signature = (<?> "type signature") . locate
  $ Anno <$> (quantified sig <|> sig)
  where sig = grouped functionType

typeDefType :: Parser Anno
typeDefType = locate $ Anno <$> baseType

type_ :: Parser AnType
type_ = (<?> "type") $ try functionType <|> baseType

scalarQuantifier :: Parser (Vector Text)
scalarQuantifier = V.fromList <$> (forAll *> many1 word)

quantifier :: Parser (Vector Text, Vector Text)
quantifier = both V.fromList . partitionEithers <$> (forAll *> many1 variable)
  where
  variable = do
    name <- word
    ($ name) <$> option Right (Left <$ ellipsis)

forAll :: Parser Token
forAll = match (TkOperator "@") <|> match (TkOperator "\x2200")

quantified :: Parser AnType -> Parser AnType
quantified thing = uncurry AnQuantified <$> quantifier <*> thing

ellipsis :: Parser Token
ellipsis = match (TkOperator "...") <|> match (TkOperator "\x2026")

functionType :: Parser AnType
functionType = (<?> "function type") $ choice
  [ try $ AnStackFunction
    <$> (word <* ellipsis) <*> left <*> (word <* ellipsis) <*> right
  , AnFunction <$> left <*> right
  ]
  where
  left, right :: Parser (Vector AnType)
  left = manyV baseType <* match TkArrow
  right = manyV type_

baseType :: Parser AnType
baseType = (<?> "base type") $ do
  prefix <- choice
    [ AnVar <$> word
    , vector
    , try $ grouped type_
    ]
  (<?> "") $ choice
    [ AnChoice prefix
      <$> (match (TkOperator "|") *> baseType)
    , AnOption prefix <$ match (TkOperator "?")
    , AnPair prefix
      <$> (match (TkOperator "&") *> baseType)
    , AnApp prefix
      <$> (forAll *> choice
        [ grouped (baseType `sepEndBy1V` match TkComma)
        , V.singleton <$> baseType
        ])
    , pure prefix
    ]

vector :: Parser AnType
vector = AnVector <$> between
  (match TkVectorBegin) (match TkVectorEnd) baseType
