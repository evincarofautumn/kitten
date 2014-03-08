{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse
  ( parse
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.Vector as V
import qualified Text.Parsec as Parsec

import Kitten.Def
import Kitten.Fragment
import Kitten.Import
import Kitten.Location
import Kitten.Parsec
import Kitten.Parse.Element
import Kitten.Parse.Layout
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parse.Type
import Kitten.Token (Located(..), Token)
import Kitten.Tree
import Kitten.Type (mono)
import Kitten.Util.Parsec

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Token as Token

parse
  :: String
  -> [Located]
  -> Either ParseError (Fragment ParsedTerm)
parse name tokens = do
  inserted <- Parsec.parse insertBraces name tokens
  Parsec.parse
    (setInitialPosition name inserted >> fragment)
    name inserted

setInitialPosition :: String -> [Located] -> Parser ()
setInitialPosition name [] = setPosition (newPos name 1 1)
setInitialPosition _ (Located{..} : _)
  = setPosition (locationStart locatedLocation)

fragment :: Parser (Fragment ParsedTerm)
fragment = fmap partitionElements $ many element <* eof

element :: Parser Element
element = choice
  [ DefElement <$> def
  , ImportElement <$> import_
  , TermElement <$> term
  ]

def :: Parser (Def ParsedTerm)
def = (<?> "definition") . locate $ do
  void (match Token.Def)
  name <- functionName <?> "definition name"
  anno <- signature
  bodyTerm <- locate (Compose <$> block) <?> "definition body"
  return $ \loc -> Def
    { defAnno = anno
    , defLocation = loc
    , defName = name
    , defTerm = mono bodyTerm
    }

import_ :: Parser Import
import_ = (<?> "import") . locate $ do
  void (match Token.Import)
  name <- bigWord
  return $ \loc -> Import
    { importName = name
    , importLocation = loc
    }

term :: Parser ParsedTerm
term = nonblockTerm <|> blockTerm
  where
  nonblockTerm :: Parser ParsedTerm
  nonblockTerm = locate $ choice
    [ try $ Push <$> nonblockValue
    , Call <$> functionName
    , VectorTerm <$> vector
    , try group
    , pair <$> tuple
    , mapOne toBuiltin <?> "builtin"
    , lambda
    , doElse
    ]

  blockTerm :: Parser ParsedTerm
  blockTerm = locate $ Push <$> blockValue

  group :: Parser (Location -> ParsedTerm)
  group = (<?> "group") $ Compose <$> grouped (many1V term)

  doElse :: Parser (Location -> ParsedTerm)
  doElse = (<?> "do block") $ match Token.Do *> doBody

  toFunctionOrBuiltin :: Token -> Maybe Text
  toFunctionOrBuiltin token = case token of
    Token.Builtin name -> Builtin.toText name
    Token.LittleWord name -> Just name
    Token.Operator name -> Just name
    _ -> Nothing

  doBody :: Parser (Location -> ParsedTerm)
  doBody = (<?> "do body") $ do
    name <- mapOne toFunctionOrBuiltin
    open <- many nonblockTerm
    body <- blockTerm
    else_ <- optionMaybe $ match Token.Else *> choice
      [ try . locate $ Push
        <$> locate (Function <$> locate doBody)
      , blockTerm
      ]
    let
      (name', else') = case else_ of
        Nothing -> (name, V.empty)
        Just elseBody -> (name <> "_else", V.singleton elseBody)
      name'' = case Builtin.fromText name' of
        Just builtin -> Builtin builtin
        _ -> Call name'
    return $ \loc -> let
      whole = V.concat
        [ V.fromList open
        , V.singleton body
        , else'
        , V.singleton (name'' loc)
        ]
      in Compose whole loc

  lambda :: Parser (Location -> ParsedTerm)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> locate (Compose <$> manyV term)
    , do
      names <- blocked (many littleWord)
      terms <- manyV term
      return $ \loc -> foldr
        (\lambdaName lambdaTerms -> Lambda lambdaName lambdaTerms loc)
        (Compose terms loc)
        (reverse names)
    ]

  pair :: Vector ParsedTerm -> Location -> ParsedTerm
  pair values loc = V.foldr1 (\x y -> PairTerm x y loc) values

  toBuiltin :: Token -> Maybe (Location -> ParsedTerm)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser (Vector ParsedTerm)
  tuple = grouped
    (locate (Compose <$> many1V term) `sepEndBy1V` match Token.Comma)
    <?> "tuple"

  vector :: Parser (Vector ParsedTerm)
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose <$> many1V term) `sepEndByV` match Token.Comma)
    <?> "vector"

blockValue :: Parser ParsedValue
blockValue = (<?> "function") . locate $ do
  terms <- block
  return $ \loc -> Function (Compose terms loc) loc

nonblockValue :: Parser ParsedValue
nonblockValue = choice
  [ locate (mapOne toLiteral <?> "literal")
  , unit
  ]

toLiteral :: Token -> Maybe (Location -> ParsedValue)
toLiteral (Token.Bool x) = Just $ Bool x
toLiteral (Token.Char x) = Just $ Char x
toLiteral (Token.Float x) = Just $ Float x
toLiteral (Token.Int x _) = Just $ Int x
toLiteral (Token.Text x) = Just $ String x
toLiteral _ = Nothing

unit :: Parser ParsedValue
unit = locate $ Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser (Vector ParsedTerm)
block = blocked (manyV term) <?> "block"
