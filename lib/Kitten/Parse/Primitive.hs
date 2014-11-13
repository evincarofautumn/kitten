{-# LANGUAGE LambdaCase #-}

module Kitten.Parse.Primitive
  ( blocked
  , grouped
  , mixfixName
  , named
  , nonsymbolic
  , qualified_
  , symbolic
  , underscore
  , vectored
  , word
  ) where

import Control.Applicative

import qualified Data.Vector as V

import Kitten.Name
import Kitten.Parse.Monad
import Kitten.Parsec
import Kitten.Token
import Kitten.Util.Function
import Kitten.Util.Maybe
import Kitten.Util.Parsec

blocked :: Parser a -> Parser a
blocked = between
  (match (TkBlockBegin NormalBlockHint))
  (match TkBlockEnd)

underscore :: Parser Token
underscore = match TkUnderscore

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

qualified_ :: Parser Name
qualified_ = do
  global <- maybe False (const True) <$> optionMaybe underscore
  names <- V.map fromUnqualified
    <$> ((named <|> symbolic) `sepBy1V` underscore)
  mOperatorName <- optionMaybe (underscore *> (fromUnqualified <$> symbolic))
  return $ case mOperatorName of
    Nothing -> let (qualifier, name) = (V.init names, V.last names)
      in if V.null qualifier && not global
        then Unqualified name
        else Qualified (Qualifier qualifier) name
    Just name -> if global
      then Qualified (Qualifier V.empty) name
      else Qualified (Qualifier names) name
  where
  fromUnqualified = \case
    Unqualified name -> name
    _ -> error "expected unqualified name"

vectored :: Parser a -> Parser a
vectored = between (match TkVectorBegin) (match TkVectorEnd)

word :: Parser Name
word = choice [try mixfixName, named, symbolic]
