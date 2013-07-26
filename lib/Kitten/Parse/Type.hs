module Kitten.Parse.Type
  ( signature
  ) where

import Control.Applicative

import Kitten.Anno (Anno(..), Type)
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

signature :: Parser Anno
signature = locate $ Anno <$> functionType
  where

  type_ :: Parser Type
  type_ = try functionType <|> baseType

  functionType :: Parser Type
  functionType = do
    left <- many baseType
    right <- match Token.Arrow *> many baseType
    effect <- choice
      [ match (Token.Operator "+") *> effectType
      , pure Anno.NoEffect
      ]
    return $ Anno.Function left right effect

  effectType :: Parser Type
  effectType = do
    left <- choice
      [ Anno.IOEffect <$ match Token.IOType
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
      [ Anno.Bool <$ match Token.BoolType
      , Anno.Char <$ match Token.CharType
      , Anno.Float <$ match Token.FloatType
      , Anno.Handle <$ match Token.HandleType
      , Anno.Int <$ match Token.IntType
      , Anno.Var <$> littleWord
      , vector
      , try unit
      , try $ grouped type_
      , tuple
      ]
    choice
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

  tuple :: Parser Type
  tuple = do
    types <- grouped (type_ `sepEndBy1` match Token.Comma)
    return $ foldr Anno.Pair Anno.Unit types

  unit :: Parser Type
  unit = Anno.Unit
    <$ (match Token.GroupBegin >> match Token.GroupEnd)
