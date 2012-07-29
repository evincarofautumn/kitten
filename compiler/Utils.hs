{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Applicative ((*>))
import Control.Monad
import Text.Parsec

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True = Just
boolToMaybe False = const Nothing

-- | Parser combinator for skipping zero or more of some
-- parser until another parser succeeds.
skipManyTill
  :: ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
