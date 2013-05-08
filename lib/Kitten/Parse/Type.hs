module Kitten.Parse.Type
  ( signature
  ) where

import Control.Applicative

import Kitten.Anno (Anno(..), Type((:>)))
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Parse.Primitive

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

signature :: Parser Anno
signature = locate $ Anno <$> signature'
  where

  signature' = do
    left <- many baseType
    mRight <- optionMaybe $ match Token.Arrow *> many baseType
    return $ case mRight of
      Just right
        -> Anno.Composition left
        :> Anno.Composition right
      Nothing
        -> Anno.Composition []
        :> Anno.Composition left

  baseType = (<?> "base type") $ choice
    [ Anno.Vector <$> between
      (match Token.VectorBegin)
      (match Token.VectorEnd)
      signature'
    , grouped signature'
    , Anno.Bool <$ match Token.BoolType
    , Anno.Text <$ match Token.TextType
    , Anno.Float <$ match Token.FloatType
    , Anno.Int <$ match Token.IntType
    , Anno.Any <$ identifier
    ]
