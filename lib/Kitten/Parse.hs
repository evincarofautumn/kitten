{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse
  ( parse
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
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
import Kitten.Term
import Kitten.TypeDef
import Kitten.Token (Located(..), Token)
import Kitten.Util.Parsec

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Token as Token

parse
  :: String
  -> [Located]
  -> Either ParseError (Fragment Value Term)
parse name
  = Parsec.parse insertBraces name
  >=> Parsec.parse fragment name

fragment :: Parser (Fragment Value Term)
fragment = partitionElements <$> (many element <* eof)

element :: Parser Element
element = choice
  [ DefElement <$> def
  , ImportElement <$> import_
  , TermElement <$> term
  , TypeElement <$> type_
  ]

def :: Parser (Def Value)
def = (<?> "definition") . locate $ do
  void (match Token.Def)
  name <- functionName <?> "definition name"
  anno <- optionMaybe signature
  body <- block <?> "definition body"
  return $ \ loc -> Def
    { defName = name
    , defTerm = Function body loc
    , defAnno = anno
    , defLocation = loc
    }

import_ :: Parser Import
import_ = (<?> "import") . locate $ do
  void (match Token.Import)
  name <- bigWord
  return $ \ loc -> Import
    { importName = name
    , importLocation = loc
    }

term :: Parser Term
term = nonblockTerm <|> blockTerm
  where
  nonblockTerm :: Parser Term
  nonblockTerm = locate $ choice
    [ try $ Push <$> nonblockValue
    , Call <$> functionName
    , VectorTerm <$> vector
    , try group
    , pair <$> tuple
    , mapOne toBuiltin <?> "builtin"
    , lambda
    , doElse
    , to
    , from
    ]

  blockTerm :: Parser Term
  blockTerm = locate $ Push <$> blockValue

  group :: Parser (Location -> Term)
  group = (<?> "group") $ Compose <$> grouped (many1V term)

  doElse :: Parser (Location -> Term)
  doElse = (<?> "do block") $ match Token.Do *> doBody

  toFunctionOrBuiltin :: Token -> Maybe Text
  toFunctionOrBuiltin token = case token of
    Token.Builtin name -> Builtin.toText name
    Token.LittleWord name -> Just name
    Token.Operator name -> Just name
    _ -> Nothing

  doBody :: Parser (Location -> Term)
  doBody = (<?> "do body") $ do
    name <- mapOne toFunctionOrBuiltin
    open <- many nonblockTerm
    body <- blockTerm
    else_ <- optionMaybe $ match Token.Else *> choice
      [ try . locate $ Push
        <$> locate (Function . V.singleton <$> locate doBody)
      , blockTerm
      ]
    let
      (name', else') = case else_ of
        Nothing -> (name, V.empty)
        Just elseBody -> (name <> "_else", V.singleton elseBody)
      name'' = case Builtin.fromText name' of
        Just builtin -> Builtin builtin
        _ -> Call name'
    return $ \ loc -> flip Compose loc $ V.concat
      [ V.fromList open
      , V.singleton body
      , else'
      , V.singleton (name'' loc)
      ]

  lambda :: Parser (Location -> Term)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> locate (Compose <$> manyV term)
    , do
      names <- blocked (many littleWord)
      terms <- manyV term
      return $ \ loc -> foldr
        (\ lambdaName lambdaTerms -> Lambda lambdaName lambdaTerms loc)
        (Compose terms loc)
        (reverse names)
    ]

  pair :: Vector Term -> Location -> Term
  pair values loc = V.foldr1 (\ x y -> PairTerm x y loc) values

  to :: Parser (Location -> Term)
  to = To <$> (match Token.To *> bigWord)

  from :: Parser (Location -> Term)
  from = From <$> (match Token.From *> bigWord)

  toBuiltin :: Token -> Maybe (Location -> Term)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser (Vector Term)
  tuple = grouped
    (locate (Compose <$> many1V term) `sepEndBy1V` match Token.Comma)
    <?> "tuple"

  vector :: Parser (Vector Term)
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose <$> many1V term) `sepEndByV` match Token.Comma)
    <?> "vector"

type_ :: Parser TypeDef
type_ = (<?> "type definition") . locate $ do
  void (match Token.Type)
  name <- bigWord
  anno <- typeDefType
  return $ \ loc -> TypeDef
    { typeDefName = name
    , typeDefAnno = anno
    , typeDefLocation = loc
    }

blockValue :: Parser Value
blockValue = (<?> "function") . locate $ Function <$> block

nonblockValue :: Parser Value
nonblockValue = choice
  [ locate (mapOne toLiteral <?> "literal")
  , unit
  ]

toLiteral :: Token -> Maybe (Location -> Value)
toLiteral (Token.Bool x) = Just $ Bool x
toLiteral (Token.Char x) = Just $ Char x
toLiteral (Token.Float x) = Just $ Float x
toLiteral (Token.Int x _) = Just $ Int x
toLiteral (Token.Text x) = Just $ \ loc
  -> Vector (V.fromList (map (`Char` loc) (T.unpack x))) loc
toLiteral _ = Nothing

unit :: Parser Value
unit = locate $ Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser (Vector Term)
block = blocked (manyV term) <?> "block"
