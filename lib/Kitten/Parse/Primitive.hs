module Kitten.Parse.Primitive
  ( bigWord
  , blocked
  , grouped
  , littleWord
  , operator
  , word
  ) where

import Control.Applicative
import Data.Text (Text)

import Kitten.Parse.Monad
import Kitten.Parsec

import qualified Kitten.Token as Token

blocked :: Parser a -> Parser a
blocked = between
  (match (Token.BlockBegin Token.NormalBlockHint))
  (match Token.BlockEnd)

littleWord :: Parser Text
littleWord = mapOne $ \token -> case token of
  Token.LittleWord name -> Just name
  _ -> Nothing

operator :: Parser Text
operator = mapOne $ \token -> case token of
  Token.Operator name -> Just name
  _ -> Nothing

bigWord :: Parser Text
bigWord = mapOne $ \token -> case token of
  Token.BigWord name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)

word :: Parser Text
word = littleWord <|> operator
