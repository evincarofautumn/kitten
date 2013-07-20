module Kitten.Parse.Primitive
  ( bigWord
  , blocked
  , functionName
  , grouped
  , littleWord
  , operator
  ) where

import Control.Applicative

import Kitten.Parse.Monad
import Kitten.Parsec

import qualified Kitten.Token as Token

blocked :: Parser a -> Parser a
blocked = between
  (match Token.BlockBegin)
  (match Token.BlockEnd)

functionName :: Parser String
functionName = littleWord <|> operator

littleWord :: Parser String
littleWord = mapOne $ \ token -> case token of
  Token.LittleWord name -> Just name
  _ -> Nothing

operator :: Parser String
operator = mapOne $ \ token -> case token of
  Token.Operator name -> Just name
  _ -> Nothing

bigWord :: Parser String
bigWord = mapOne $ \ token -> case token of
  Token.BigWord name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)
