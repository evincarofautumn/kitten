{-|
Module      : Kitten.Parser
Description : Parsing utilities
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Parser
  ( Layouter
  , Parser
  , getTokenOrigin
  , parserMatch
  , parserMatch_
  , tokenSatisfy
  ) where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Kitten.Layoutness (Layoutness(..))
import Kitten.Located (Located)
import Kitten.Name (Qualifier)
import Kitten.Origin (Origin)
import Kitten.Token (Token)
import Text.Parsec ((<?>), ParsecT)
import Text.Parsec.Pos (SourcePos)
import qualified Kitten.Located as Located
import qualified Kitten.Origin as Origin
import qualified Text.Parsec as Parsec

type Layouter a = GeneralParser 'Layout a
type Parser a = GeneralParser 'Nonlayout a
type GeneralParser l a = ParsecT [Located (Token l)] Qualifier Identity a

getTokenOrigin :: GeneralParser l Origin
getTokenOrigin = Located.origin
  <$> Parsec.lookAhead (tokenSatisfy (const True))

tokenSatisfy
  :: (Located (Token l) -> Bool)
  -> GeneralParser l (Located (Token l))
tokenSatisfy predicate = Parsec.tokenPrim show advance
  (\token -> if predicate token then Just token else Nothing)
  where

    advance
      :: SourcePos
      -> Located (Token l)
      -> [Located (Token l)]
      -> SourcePos
    advance _ _ (token : _) = Origin.begin (Located.origin token)
    advance sourcePos _ _ = sourcePos

parserMatch :: Token l -> GeneralParser l (Located (Token l))
parserMatch token = tokenSatisfy ((== token) . Located.item) <?> show token

parserMatch_ :: Token l -> GeneralParser l ()
parserMatch_ = void . parserMatch
