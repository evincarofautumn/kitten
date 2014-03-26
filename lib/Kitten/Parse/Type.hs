{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( signature
  , typeDefType
  ) where

import Control.Applicative
import Data.Either
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Anno (Anno(..), Type)
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive
import Kitten.Token (Token)
import Kitten.Util.Parsec

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

signature :: Parser Anno
signature = (<?> "type signature") . locate
  $ Anno <$> (quantified sig <|> sig)
  where sig = grouped functionType

typeDefType :: Parser Anno
typeDefType = locate $ Anno <$> baseType

type_ :: Parser Type
type_ = (<?> "type") $ try functionType <|> baseType

quantified :: Parser Type -> Parser Type
quantified thing = do
  (stacks, scalars) <- partitionEithers <$> between
    (match $ Token.BlockBegin Token.NormalBlockHint)
    (match Token.BlockEnd)
    (variable `sepEndBy1` match Token.Comma)
  Anno.Quantified (V.fromList stacks) (V.fromList scalars) <$> thing
  where
  variable :: Parser (Either Text Text)
  variable = Left <$> (dot *> word) <|> Right <$> word

dot :: Parser Token
dot = match (Token.Operator ".")

functionType :: Parser Type
functionType = (<?> "function type") $ choice
  [ Anno.StackFunction <$> (dot *> word) <*> left <*> (dot *> word) <*> right
  , Anno.Function <$> left <*> right
  ]
  where
  left, right :: Parser (Vector Type)
  left = manyV baseType <* match Token.Arrow
  right = manyV type_

baseType :: Parser Type
baseType = (<?> "base type") $ do
  prefix <- choice
    [ Anno.Bool <$ match (Token.Word "Bool")
    , Anno.Char <$ match (Token.Word "Char")
    , Anno.Float <$ match (Token.Word "Float")
    , Anno.Handle <$ match (Token.Word "Handle")
    , Anno.Int <$ match (Token.Word "Int")
    , Anno.Var <$> word
    , vector
    , try $ grouped type_
    ]
  (<?> "") $ choice
    [ Anno.Choice prefix
      <$> (match (Token.Operator "|") *> baseType)
    , Anno.Option prefix <$ match (Token.Operator "?")
    , Anno.Pair prefix
      <$> (match (Token.Operator "&") *> baseType)
    , pure prefix
    ]

vector :: Parser Type
vector = Anno.Vector <$> between
  (match Token.VectorBegin)
  (match Token.VectorEnd)
  baseType
