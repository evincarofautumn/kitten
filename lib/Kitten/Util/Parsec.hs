module Kitten.Util.Parsec
  ( many1V
  , manyV
  , sepBy1V
  , sepEndBy1V
  , sepEndByV
  , skipManyTill
  ) where

import Control.Monad
import Control.Applicative
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Parsec

many1V
  :: (Stream s m t)
  => ParsecT s u m a
  -> ParsecT s u m (Vector a)
many1V = liftM V.fromList . many1

manyV
  :: (Monad m)
  => ParsecT s u m a
  -> ParsecT s u m (Vector a)
manyV = liftM V.fromList . many

sepBy1V
  :: (Stream s m t)
  => ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m (Vector a)
sepBy1V = (liftM V.fromList .) . sepBy1

sepEndBy1V
  :: (Stream s m t)
  => ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m (Vector a)
sepEndBy1V = (liftM V.fromList .) . sepEndBy1

sepEndByV
  :: (Stream s m t)
  => ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m (Vector a)
sepEndByV = (liftM V.fromList .) . sepEndBy

skipManyTill
  :: ParsecT s u m a
  -> ParsecT s u m b
  -> ParsecT s u m ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
