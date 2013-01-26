module Kitten.Term
  ( Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Data.Either
import Data.List
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec
  hiding ((<|>), Empty, many, parse, satisfy, token, tokens)

import qualified Data.Vector as Vector
import qualified Text.Parsec as Parsec

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Token (Located(..), Token)

import qualified Kitten.Token as Token

type Parser a = ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !Text !Term
  | Compose !Term !Term
  | Empty

data Value
  = Word !Text
  | Int !Int
  | Bool !Bool
  | Text !Text
  | Vec !(Vector Value)
  | Fun !Term

parse :: String -> [Located] -> Either ParseError (Fragment Term)
parse = Parsec.parse programP

programP :: Parser (Fragment Term)
programP = uncurry Fragment . (Vector.fromList *** compose) . partitionEithers
  <$> many ((Left <$> defP) <|> (Right <$> termP)) <* eof

compose :: [Term] -> Term
compose = foldl' Compose Empty

defP :: Parser (Def Term)
defP = (<?> "definition") $ do
  void $ token Token.Def
  mParams <- optionMaybe $ groupedP identifierP
  name <- identifierP
  body <- oneOrGroupP
  let
    body' = case mParams of
      Just params -> foldr Lambda body $ reverse params
      Nothing -> body
  return $ Def name body'

termP :: Parser Term
termP = choice [Value <$> valueP, builtinP, lambdaP]

valueP :: Parser Value
valueP = choice
  [ literalP
  , vecP
  , funP
  , wordP
  ]
  where
  literalP = mapOne toLiteral <?> "literal"
  toLiteral (Token.Int value) = Just $ Int value
  toLiteral (Token.Bool value) = Just $ Bool value
  toLiteral (Token.Text value) = Just $ Text value
  toLiteral _ = Nothing
  vecP = Vec . Vector.fromList
    <$> (token Token.VecBegin *> many valueP <* token Token.VecEnd)
    <?> "vector"
  wordP = mapOne toWord <?> "word"
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing
  funP = Fun . compose
    <$> (groupedP termP <|> layoutP)
    <?> "function"

groupedP :: Parser a -> Parser [a]
groupedP parser = token Token.FunBegin *> many parser <* token Token.FunEnd

layoutP :: Parser [Term]
layoutP = do
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
  case Parsec.parse (many termP) "layout quotation" tokens of
    Right result -> return result
    Left err -> fail $ show err

identifierP :: Parser Text
identifierP = mapOne toIdentifier
  where
  toIdentifier (Token.Word name) = Just name
  toIdentifier _ = Nothing

builtinP :: Parser Term
builtinP = mapOne toBuiltin <?> "builtin"
  where
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

lambdaP :: Parser Term
lambdaP = (<?> "lambda") $ do
  name <- token Token.Lambda *> identifierP
  Lambda name <$> oneOrGroupP

oneOrGroupP :: Parser Term
oneOrGroupP = choice
  [ compose <$> groupedP termP
  , compose <$> layoutP
  , termP
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

justIf :: Bool -> a -> Maybe a
justIf c x = if c then Just x else Nothing

locatedSatisfy
  :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token
token tok = satisfy (== tok)

located :: Token -> Parser Located
located tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocated :: Parser Located
anyLocated = locatedSatisfy (const True)
