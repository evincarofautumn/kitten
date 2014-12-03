{-# LANGUAGE OverloadedStrings #-}

module Kitten.Parse.Type
  ( baseType
  , scalarQuantifier
  , signature
  , typeDefType
  , type_
  ) where

import Control.Applicative
import Data.Either
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.Annotation
import Kitten.Location
import Kitten.Name
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parsec
import Kitten.Token
import Kitten.Util.Parsec
import Kitten.Util.Tuple

signature :: Parser Anno
signature = (<?> "type signature")
  $ Anno <$> (quantified sig <|> sig)
  where sig = grouped functionType

typeDefType :: Parser Anno
typeDefType = Anno <$> baseType

type_ :: Parser AnType
type_ = (<?> "type") $ try functionType <|> baseType

scalarQuantifier :: Parser (Vector (Name, Location))
scalarQuantifier = V.fromList
  <$> angled (var `sepEndBy1` comma)
  where
  var = do
    loc <- getLocation
    (,) <$> word <*> pure loc

quantifier :: Parser (Vector (Name, Location), Vector (Name, Location))
quantifier = both V.fromList . partitionEithers
  <$> angled (var `sepEndBy1` comma)
  where
  var = do
    loc <- getLocation
    name <- word
    option (Right (name, loc)) (Left (name, loc) <$ ellipsis)

angled :: Parser a -> Parser a
angled = between leftAngle rightAngle
  where
  leftAngle = match (TkOperator "<")
  rightAngle = match (TkOperator ">")

quantified :: Parser AnType -> Parser AnType
quantified thing = do
  loc <- getLocation
  uncurry AnQuantified <$> quantifier <*> thing <*> pure loc

ellipsis :: Parser Token
ellipsis = match TkEllipsis

functionType :: Parser AnType
functionType = (<?> "function type") $ choice
  [ try $ do
    leftVar <- word <* ellipsis
    leftTypes <- left
    loc <- arrow
    rightVar <- word <* ellipsis
    rightTypes <- right
    return $ AnStackFunction leftVar leftTypes rightVar rightTypes loc
  , do
    leftTypes <- left
    loc <- arrow
    rightTypes <- right
    return $ AnFunction leftTypes rightTypes loc
  ]
  where
  left = manyV baseType
  right = manyV type_
  arrow = getLocation <* match TkArrow

baseType :: Parser AnType
baseType = (<?> "base type") $ do
  prefix <- choice
    [ quantified (grouped type_)
    , do
      loc <- getLocation
      AnVar <$> qualified_ <*> pure loc
    , vector
    , grouped type_
    ]
  (<?> "") $ choice
    [ do
      loc <- getLocation <* match (TkOperator "|")
      AnChoice prefix <$> baseType <*> pure loc
    , do
      loc <- getLocation <* match (TkOperator "?")
      return $ AnOption prefix loc
    , do
      loc <- getLocation <* match (TkOperator "&")
      AnPair prefix <$> baseType <*> pure loc
    , do
      (loc, args) <- (,) <$> (getLocation <* forAll) <*> choice
        [ grouped (baseType `sepEndBy1V` comma)
        , V.singleton <$> baseType
        ]
      return $ AnApp prefix args loc
    , pure prefix
    ]

forAll :: Parser Token
forAll = match (TkOperator "@") <|> match (TkOperator "\x2200")

comma :: Parser Token
comma = match TkComma

vector :: Parser AnType
vector = do
  loc <- getLocation
  AnVector <$> between
    (match TkVectorBegin) (match TkVectorEnd) type_ <*> pure loc
