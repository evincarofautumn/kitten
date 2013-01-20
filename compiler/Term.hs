module Term
  ( Def(..)
  , Program(..)
  , Term(..)
  , Value(..)
  , parse
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Data.Either
import Data.List
import Text.Parsec ((<?>))

import qualified Text.Parsec as P

import Builtin (Builtin)
import Def
import Program
import Token (Located(..), Token)

import qualified Token

type Parser a = P.ParsecT [Located] () Identity a

data Term
  = Value !Value
  | Builtin !Builtin
  | Lambda !String !Term
  | Compose !Term !Term
  | Empty

data Value
  = Word !String
  | Int !Int
  | Bool !Bool
  | Vec ![Value]
  | Fun !Term

parse :: String -> [Located] -> Either P.ParseError (Program Term)
parse = P.parse programP

programP :: Parser (Program Term)
programP = uncurry Program . second compose . partitionEithers
  <$> P.many ((Left <$> defP) <|> (Right <$> termP)) <* P.eof

compose :: [Term] -> Term
compose = foldl' Compose Empty

defP :: Parser (Def Term)
defP = (<?> "definition") $ do
  name <- token Token.Def *> identifierP
  Def name <$> oneOrGroupP

termP :: Parser Term
termP = P.choice [Value <$> valueP, builtinP, lambdaP]

valueP :: Parser Value
valueP = P.choice
  [ literalP
  , vecP
  , funP
  , wordP
  ]
  where
  literalP = mapOne toLiteral <?> "literal"
  toLiteral (Token.Int value) = Just $ Int value
  toLiteral (Token.Bool value) = Just $ Bool value
  toLiteral _ = Nothing
  vecP = Vec <$> (token Token.VecBegin *> many valueP <* token Token.VecEnd)
    <?> "vector"
  wordP = mapOne toWord <?> "word"
  toWord (Token.Word name) = Just $ Word name
  toWord _ = Nothing
  funP = Fun . compose
    <$> (groupedP <|> layoutP)
    <?> "function"

groupedP :: Parser [Term]
groupedP = token Token.FunBegin *> many termP <* token Token.FunEnd

layoutP :: Parser [Term]
layoutP = do
  Token.Located startLocation _ _ <- located Token.Layout
  tokens <- many . locatedSatisfy $ \ (Token.Located location _ _)
    -> P.sourceColumn location > P.sourceColumn startLocation
  case P.parse (many termP) "layout quotation" tokens of
    Right result -> return result
    Left err -> fail $ show err

identifierP :: Parser String
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
oneOrGroupP = P.choice
  [ compose <$> groupedP
  , compose <$> layoutP
  , termP
  ]

advance :: P.SourcePos -> t -> [Located] -> P.SourcePos
advance _ _ (Located sourcePos _ _ : _) = sourcePos
advance sourcePos _ _ = sourcePos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = P.tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> justIf (f t) t

mapOne :: (Token -> Maybe a) -> Parser a
mapOne f = P.tokenPrim show advance
  $ \ Located { Token.locatedToken = t } -> f t

justIf :: Bool -> a -> Maybe a
justIf c x = if c then Just x else Nothing

locatedSatisfy
  :: (Located -> Bool) -> Parser Located
locatedSatisfy predicate = P.tokenPrim show advance
  $ \ loc -> justIf (predicate loc) loc

token :: Token -> Parser Token -- P.ParsecT s u m Token
token tok = satisfy (== tok)

located :: Token -> Parser Located
located tok = locatedSatisfy (\ (Located _ _ loc) -> loc == tok)

anyLocatedToken :: Parser Located
anyLocatedToken = locatedSatisfy (const True)
