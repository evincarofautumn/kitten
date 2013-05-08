module Kitten.Util.Parsec
  ( skipManyTill
  ) where

import Control.Monad
import Control.Applicative

import Kitten.Parsec

skipManyTill
  :: ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
