module Kitten.Parse.Type
  ( signature
  ) where

import Control.Applicative

import Kitten.Anno (Anno(..), Type((:>)))
import Kitten.Kind
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

signature :: Parser Anno
signature = locate $ Anno <$> functionType
  where

  functionType :: Parser (Type Scalar)
  functionType = do
    left <- many baseType
    mRight <- case left of
      [] -> Just <$> (match Token.Arrow *> many baseType)
      _ -> optionMaybe $ match Token.Arrow *> many baseType
    return $ case mRight of
      Just right
        -> Anno.Composition left
        :> Anno.Composition right
      Nothing
        -> Anno.Composition []
        :> Anno.Composition left

  baseType :: Parser (Type Scalar)
  baseType = (<?> "base type") $ choice
    [ Anno.Bool <$ match Token.BoolType
    , Anno.Char <$ match Token.CharType
    , Anno.Float <$ match Token.FloatType
    , Anno.Int <$ match Token.IntType
    , Anno.Any <$ littleWord
    , vector
    , try $ grouped functionType
    , try tuple
    , unit
    ]

  vector :: Parser (Type Scalar)
  vector = Anno.Vector <$> between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    baseType

  tuple :: Parser (Type Scalar)
  tuple = Anno.Tuple <$> grouped
    (baseType `sepEndBy` match Token.Comma)

  unit :: Parser (Type Scalar)
  unit = Anno.Unit
    <$ (match Token.GroupBegin >> match Token.GroupEnd)
