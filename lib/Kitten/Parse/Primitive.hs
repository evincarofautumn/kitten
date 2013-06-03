module Kitten.Parse.Primitive
  ( bigWord
  , blocked
  , grouped
  , littleWord
  ) where

import Kitten.Parse.Monad
import Kitten.Parsec

import qualified Kitten.Token as Token

blocked :: Parser a -> Parser a
blocked = between
  (match Token.BlockBegin)
  (match Token.BlockEnd)

littleWord :: Parser String
littleWord = mapOne $ \ token -> case token of
  Token.LittleWord name -> Just name
  _ -> Nothing

bigWord :: Parser String
bigWord = mapOne $ \ token -> case token of
  Token.BigWord name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)
