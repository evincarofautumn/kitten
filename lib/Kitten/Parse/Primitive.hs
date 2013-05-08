module Kitten.Parse.Primitive
  ( blocked
  , identifier
  , grouped
  ) where

import Kitten.Parse.Monad
import Kitten.Parsec

import qualified Kitten.Token as Token

blocked :: Parser a -> Parser a
blocked = between
  (match Token.BlockBegin)
  (match Token.BlockEnd)

identifier :: Parser String
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)
