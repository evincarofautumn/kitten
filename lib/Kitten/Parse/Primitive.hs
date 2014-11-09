{-# LANGUAGE LambdaCase #-}

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
import Kitten.Token

blocked :: Parser a -> Parser a
blocked = between
  (match (TkBlockBegin NormalBlockHint))
  (match TkBlockEnd)

symbolic :: Parser Text
symbolic = mapOne $ \case
  TkOperator name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between (match TkGroupBegin) (match TkGroupEnd)

named :: Parser Text
named = mapOne $ \case
  TkWord name -> Just name
  _ -> Nothing

word :: Parser Text
word = named <|> symbolic
