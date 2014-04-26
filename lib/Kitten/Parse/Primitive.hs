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
import Kitten.Types

blocked :: Parser a -> Parser a
blocked = between
  (match (TkBlockBegin NormalBlockHint))
  (match TkBlockEnd)

symbolic :: Parser Text
symbolic = mapOne $ \token -> case token of
  TkOperator name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between
  (match TkGroupBegin)
  (match TkGroupEnd)

named :: Parser Text
named = mapOne $ \token -> case token of
  TkWord name -> Just name
  _ -> Nothing

word :: Parser Text
word = named <|> symbolic
