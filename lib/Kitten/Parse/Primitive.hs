module Kitten.Parse.Primitive
  ( blocked
  , grouped
  , named
  , symbolic
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

symbolic :: Parser Text
symbolic = mapOne $ \token -> case token of
  Token.Operator name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match Token.GroupBegin)
  (match Token.GroupEnd)

named :: Parser Text
named = mapOne $ \token -> case token of
  Token.Word name -> Just name
  _ -> Nothing

word :: Parser Text
word = named <|> symbolic
