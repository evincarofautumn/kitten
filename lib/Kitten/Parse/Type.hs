{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( baseType
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

signature :: Parser Anno
signature = (<?> "type signature") . locate
  $ Anno <$> (quantified sig <|> sig)
  where sig = grouped functionType

typeDefType :: Parser Anno
typeDefType = locate $ Anno <$> baseType

type_ :: Parser AnType
type_ = (<?> "type") $ try functionType <|> baseType

quantified :: Parser AnType -> Parser AnType
quantified thing = do
  (stacks, scalars) <- partitionEithers <$> between
    (match $ TkBlockBegin NormalBlockHint)
    (match TkBlockEnd)
    (variable `sepEndBy1` match TkComma)
  AnQuantified (V.fromList stacks) (V.fromList scalars) <$> thing
  where
  variable :: Parser (Either Text Text)
  variable = Left <$> (dot *> word) <|> Right <$> word

dot :: Parser Token
dot = match (TkOperator ".")

functionType :: Parser AnType
functionType = (<?> "function type") $ choice
  [ AnStackFunction <$> (dot *> word) <*> left <*> (dot *> word) <*> right
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
    , pure prefix
    ]

vector :: Parser AnType
vector = AnVector <$> between
  (match TkVectorBegin) (match TkVectorEnd) baseType
