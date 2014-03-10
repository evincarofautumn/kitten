{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse.Monad
  ( Parser
  , advance
  , locate
  , locatedMatch
  , locatedSatisfy
  , mapOne
  , match
  , satisfy
  ) where

import Data.Functor.Identity

import Kitten.Location
import Kitten.Parsec
import Kitten.Token (Located(..), Token)
import Kitten.Util.Maybe

type Parser a = ParsecT [Located] () Identity a

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located _ Location{..} : _) = locationStart
advance sourcePos _ _ = sourcePos

locate
  :: (Stream s m c, Monad m)
  => ParsecT s u m (Location -> a)
  -> ParsecT s u m a
locate parser = do
  start <- getPosition
  result <- parser
  return $ result Location
    { locationStart = start
    , locationIndent = -1  -- FIXME
    }

locatedMatch :: Token -> Parser Located
locatedMatch token
  = locatedSatisfy ((token ==) . locatedToken)

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \located -> justIf (predicate located) located

mapOne :: (Token -> Maybe a) -> Parser a
mapOne extract = tokenPrim show advance
  $ \(Located locatedToken _) -> extract locatedToken

match :: Token -> Parser Token
match token = satisfy (== token) <?> show token

satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = tokenPrim show advance $ \Located{..}
  -> justIf (predicate locatedToken) locatedToken
