{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( signature
  , typeDefType
  ) where

import Control.Applicative

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
    <*> pure Anno.NoEffect
  , grouped functionType
  ]

typeDefType :: Parser Anno
typeDefType = locate $ Anno <$> baseType

type_ :: Parser Type
type_ = (<?> "type") $ try functionType <|> baseType

functionType :: Parser Type
functionType = (<?> "function type") $ do
  left <- manyV baseType
  right <- match Token.Arrow *> manyV type_
  effect <- choice
    [ match (Token.Operator "+") *> effectType
    , pure Anno.NoEffect
    ]
  return $ Anno.Function left right effect

effectType :: Parser Type
effectType = (<?> "effect type") $ do
  left <- choice
    [ Anno.IOEffect <$ match (Token.BigWord "IO")
    , Anno.Var <$> littleWord
    , try $ Anno.NoEffect <$ unit
    , grouped effectType
    ]
  choice
    [ Anno.Join left
      <$> (match (Token.Operator "+") *> effectType)
    , pure left
    ]

baseType :: Parser Type
baseType = (<?> "base type") $ do
  prefix <- choice
    [ Anno.Bool <$ match (Token.BigWord "Bool")
    , Anno.Char <$ match (Token.BigWord "Char")
    , Anno.Float <$ match (Token.BigWord "Float")
    , Anno.Handle <$ match (Token.BigWord "Handle")
    , Anno.Int <$ match (Token.BigWord "Int")
    , Anno.Var <$> littleWord
    , Anno.Named <$> bigWord
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
