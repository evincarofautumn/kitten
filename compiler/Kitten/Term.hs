module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Monad.Identity
import Data.List
import Data.Monoid
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Text.Parsec as Parsec

import Kitten.Anno (Anno(..), Type((:>), (:.)))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Token (Located(..), Token)
import Kitten.Util

import qualified Kitten.Anno as Anno
import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !Text !Term
  | Compose !(Vector Term)

data Value
  = Word !Text
  | Int !Int
  | Bool !Bool
  | Text !Text
  | Vec !(Vector Value)
  | Tuple !(Vector Value)
  | Fun !Term

data Element
  = AnnoElement !Anno
  | DefElement !(Def Term)
  | TermElement !Term

parse :: String -> [Located] -> Either ParseError (Fragment Term)
parse = Parsec.parse fragment

partitionElements :: [Element] -> ([Anno], [Def Term], [Term])
partitionElements = foldr partitionElement mempty
  where
  partitionElement (AnnoElement a) (as, ds, ts) = (a : as, ds, ts)
  partitionElement (DefElement d) (as, ds, ts) = (as, d : ds, ts)
  partitionElement (TermElement t) (as, ds, ts) = (as, ds, t : ts)

fragment :: Parser (Fragment Term)
fragment = do
  elements <- many element <* eof
  let (_annos, defs, terms) = partitionElements elements
  return $ Fragment (Vector.fromList defs) (compose terms)

element :: Parser Element
element = choice
  [ AnnoElement <$> anno
  , DefElement <$> def
  , TermElement <$> term
  ]

compose :: [Term] -> Term
compose = Compose . Vector.fromList

anno :: Parser Anno
anno = do
  void $ token Token.Type
  params <- maybe mempty makeParamMap
    <$> optionMaybe (grouped $ many identifier)
  name <- identifier
  rawType <- block (sig params) <|> layout (sig params)
  return $ Anno name
    (Set.fromList [Name 0 .. Name $ Map.size params])
    rawType
  where makeParamMap = Map.fromList . flip zip [Name 0 ..]

sig :: Map Text Name -> Parser Type
sig params = sig'
  where
  sig' = do
    left <- many1 baseType
    mRight <- optionMaybe $ token Token.Arrow *> many1 baseType
    case mRight of
      Just right -> return $ composeTypes left :> composeTypes right
      Nothing -> return $ composeTypes left
  baseType = choice
    [ Anno.Vec <$> between (token Token.VecBegin) (token Token.VecEnd) sig'
    , Anno.Tuple . Vector.fromList <$> grouped (many baseType)
    , block sig'
    , var
    ]
  var = do
    word <- identifier
    return $ case Map.lookup word params of
      Just param -> Anno.Var param
      Nothing -> Anno.Word word
  composeTypes = foldl' (:.) Anno.Empty

def :: Parser (Def Term)
def = (<?> "definition") $ do
  void $ token Token.Def
  mParams <- optionMaybe . grouped $ many identifier
  name <- identifier
  body <- oneOrBlock
  let
    body' = case mParams of
      Just params -> foldr Lambda body $ reverse params
      Nothing -> body
  return $ Def name body'

term :: Parser Term
term = choice
  [ Value <$> value
  , mapOne toBuiltin <?> "builtin"
  , lambda
  ]
  where
  lambda = (<?> "lambda") $ do
    name <- token Token.Lambda *> identifier
    Lambda name <$> oneOrBlock
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

value :: Parser Value
value = choice
  [ mapOne toLiteral <?> "literal"
  , mapOne toWord <?> "word"
  , vec
  , fun
  , tuple
  ]
  where
  toLiteral (Token.Int x) = Just $ Int x
  toLiteral (Token.Bool x) = Just $ Bool x
  toLiteral (Token.Text x) = Just $ Text x
  toLiteral _ = Nothing
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing
  vec = Vec . Vector.reverse . Vector.fromList
    <$> between (token Token.VecBegin) (token Token.VecEnd) (many value)
    <?> "vector"
  fun = Fun . compose
    <$> (block (many term) <|> layout (many term))
    <?> "function"
  tuple = Tuple . Vector.reverse . Vector.fromList
    <$> between (token Token.TupleBegin) (token Token.TupleEnd) (many value)
    <?> "tuple"

grouped :: Parser a -> Parser a
grouped = between (token Token.TupleBegin) (token Token.TupleEnd)

block :: Parser a -> Parser a
block = between (token Token.FunBegin) (token Token.FunEnd)

layout :: Parser a -> Parser a
layout inner = do
  Token.Located startLocation startIndent _ <- located Token.Layout
  firstToken@(Token.Located firstLocation _ _) <- anyLocated
  let
    inside = if sourceLine firstLocation == sourceLine startLocation
      then \ (Token.Located location _ _)
        -> sourceColumn location > sourceColumn startLocation
      else \ (Token.Located location _ _)
        -> sourceColumn location > startIndent
  innerTokens <- many $ locatedSatisfy inside
  let tokens = firstToken : innerTokens
  case Parsec.parse (inner <* eof) "layout" tokens of
    Right result -> return result
    Left err -> fail $ show err

identifier :: Parser Text
identifier = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

oneOrBlock :: Parser Term
oneOrBlock = choice
  [ compose <$> block (many term)
  , compose <$> layout (many term)
  , term
  ]

advance :: SourcePos -> t -> [Located] -> SourcePos
advance _ _ (Located sourcePos _ _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> justIf (f t) t

mapOne :: (Token -> Maybe a) -> Parser a
mapOne f = tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> f t

locatedSatisfy :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token
token tok = satisfy (== tok)

located :: Token -> Parser Located
located tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
