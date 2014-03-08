{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( signature
  , typeDefType
  ) where

import Control.Applicative
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Anno (Anno(..), Type)
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive
import Kitten.Util.Parsec

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

signature :: Parser Anno
signature = (<?> "type signature") . locate
  $ Anno <$> choice
  [ try . grouped $ Anno.Function V.empty
    <$> (V.singleton <$> baseType)
  , grouped functionType
  ]

typeDefType :: Parser Anno
typeDefType = locate $ Anno <$> baseType

type_ :: Parser Type
type_ = (<?> "type") $ try functionType <|> baseType

functionType :: Parser Type
functionType = (<?> "function type") $ choice
  [ Anno.Function <$> left <*> right
  , Anno.RowFunction <$> row <*> left <*> row <*> right
  ]
  where

  left, right :: Parser (Vector Type)
  left = manyV baseType <* match Token.Arrow
  right = manyV type_

  row :: Parser Text
  row = match (Token.Operator ".") *> littleWord

baseType :: Parser Type
baseType = (<?> "base type") $ do
  prefix <- choice
    [ Anno.Bool <$ match (Token.BigWord "Bool")
    , Anno.Char <$ match (Token.BigWord "Char")
    , Anno.Float <$ match (Token.BigWord "Float")
    , Anno.Handle <$ match (Token.BigWord "Handle")
    , Anno.Int <$ match (Token.BigWord "Int")
    , Anno.Var <$> littleWord
    , vector
    , try unit
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

unit :: Parser Type
unit = Anno.Unit
  <$ (match Token.GroupBegin >> match Token.GroupEnd)
