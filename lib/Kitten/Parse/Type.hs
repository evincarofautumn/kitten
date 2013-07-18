module Kitten.Parse.Type
  ( signature
  ) where

import Control.Applicative

import Kitten.Anno (Anno(..), Type)
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive
import Kitten.Purity

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
    (right, purity) <- choice
      [ match Token.Arrow
        *> ((,) <$> many baseType <*> pure Pure)
      , match Token.FatArrow
        *> ((,) <$> many baseType <*> pure Impure)
      ]
    return $ Anno.Function left right purity

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
        <$> (match (Token.LittleWord "|") *> baseType)
      , Anno.Option prefix <$ match (Token.LittleWord "?")
      , Anno.Pair prefix
        <$> (match (Token.LittleWord "&") *> baseType)
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
