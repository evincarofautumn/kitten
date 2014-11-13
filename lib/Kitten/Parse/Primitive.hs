{-# LANGUAGE LambdaCase #-}

module Kitten.Parse.Primitive
  ( blocked
  , grouped
  , ignore
  , mixfixName
  , named
  , nonsymbolic
  , symbolic
  , vectored
  , word
  ) where

import Control.Applicative

import Kitten.Name
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Token
import Kitten.Util.Function
import Kitten.Util.Maybe

blocked :: Parser a -> Parser a
blocked = between
  (match (TkBlockBegin NormalBlockHint))
  (match TkBlockEnd)

ignore :: Parser Token
ignore = match TkIgnore

symbolic :: Parser Name
symbolic = mapOne $ \case
  TkOperator name -> Just name
  _ -> Nothing

grouped :: Parser a -> Parser a
grouped = between (match TkGroupBegin) (match TkGroupEnd)

mixfixName :: Parser Name
mixfixName = (MixfixName . concat) .: (:)
  <$> ((:) <$> part <*> many1 hole)
  <*> many (consMaybe <$> optionMaybe part <*> many1 hole)
  where
  hole = try $ grouped (pure MixfixHole)
  part = do
    name <- named
    case name of
      MixfixName{} -> error "mixfix name should not contain mixfix name"
      Qualified{} -> fail "qualified names are not allowed in mixfix names"
      Unqualified text -> return (MixfixNamePart text)

named :: Parser Name
named = mapOne $ \case
  TkWord name -> Just name
  _ -> Nothing

nonsymbolic :: Parser Name
nonsymbolic = try mixfixName <|> named

vectored :: Parser a -> Parser a
vectored = between (match TkVectorBegin) (match TkVectorEnd)

word :: Parser Name
word = choice [try mixfixName, named, symbolic]
