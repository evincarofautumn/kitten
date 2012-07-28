module Utils where

import Control.Applicative ((*>))
import Control.Monad
import Text.Parsec
import Text.Parsec.String

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True = Just
boolToMaybe False = const Nothing

-- | Parser combinator for skipping zero or more of some
-- parser until another parser succeeds.
skipManyTill
  :: Parser a
  -> Parser b
  -> Parser ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
