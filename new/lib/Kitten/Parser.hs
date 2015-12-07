module Kitten.Parser
  ( Parser
  , getOrigin
  , parserMatch
  , parserMatch_
  , tokenSatisfy
  ) where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Kitten.Located (Located)
import Kitten.Name (Qualifier)
import Kitten.Origin (Origin(Origin))
import Kitten.Token (Token)
import Text.Parsec ((<?>), ParsecT)
import Text.Parsec.Pos (SourcePos)
import qualified Kitten.Located as Located
import qualified Kitten.Origin as Origin
import qualified Text.Parsec as Parsec

type Parser a = ParsecT [Located Token] Qualifier Identity a

getOrigin :: (Monad m, Parsec.Stream s m c) => ParsecT s u m Origin
getOrigin = do
  start <- Parsec.getPosition
  return (Origin start start)

tokenSatisfy :: (Located Token -> Bool) -> Parser (Located Token)
tokenSatisfy predicate = Parsec.tokenPrim show advance
  (\token -> if predicate token then Just token else Nothing)
  where
  advance :: SourcePos -> t -> [Located Token] -> SourcePos
  advance _ _ (token : _) = Origin.begin (Located.origin token)
  advance sourcePos _ _ = sourcePos

parserMatch :: Token -> Parser (Located Token)
parserMatch token = tokenSatisfy ((== token) . Located.item) <?> show token

parserMatch_ :: Token -> Parser ()
parserMatch_ = void . parserMatch
