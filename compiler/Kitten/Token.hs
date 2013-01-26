module Kitten.Token
  ( Located(..)
  , Token(..)
  , tokenize
  ) where

import Control.Applicative
import Control.Monad.Identity
import Data.Text (Text)
import Text.Parsec
  hiding ((<|>), many, newline, optional, token, tokens)

import qualified Data.Text as Text

import Kitten.Builtin (Builtin)
import Kitten.Util

import qualified Kitten.Builtin as Builtin

type Parser a = ParsecT String Column Identity a

data Token
  = Word !Text
  | Builtin !Builtin
  | Int !Int
  | Bool !Bool
  | Text !Text
  | Def
  | Lambda
  | VecBegin
  | VecEnd
  | FunBegin
  | FunEnd
  | Layout
  deriving (Eq)

instance Show Token where
  show (Word word) = show word
  show (Builtin name) = show name
  show (Int value) = show value
  show (Bool value) = if value then "true" else "false"
  show (Text value) = show value
  show Def = "def"
  show Lambda = "\\"
  show VecBegin = "["
  show VecEnd = "]"
  show FunBegin = "{"
  show FunEnd = "}"
  show Layout = ":"

data Located = Located
  { locatedLocation :: SourcePos
  , locatedIndent :: Column
  , locatedToken :: Token
  }

instance Show Located where
  show Located{..} = show locatedToken

tokenize :: String -> String -> Either ParseError [Located]
tokenize = runParser file 0

located
  :: Parser Token
  -> Parser Located
located parser = do
  indent <- getState
  position <- getPosition
  result <- parser
  return $ Located position indent result

file :: Parser [Located]
file = silence *> tokens <* eof

tokens :: Parser [Located]
tokens = token `sepEndBy` silence

token :: Parser Located
token = (<?> "token") . located $ choice
  [ lambda
  , vecBegin
  , vecEnd
  , funBegin
  , funEnd
  , layout
  , try int
  , text
  , word
  ]

  where
  lambda = Lambda <$ char '\\'

  vecBegin = VecBegin <$ char '['
  vecEnd = VecEnd <$ char ']'

  funBegin = FunBegin <$ char '{'
  layout = Layout <$ char ':'
  funEnd = FunEnd <$ char '}'

  int = do
    sign <- optionMaybe $ oneOf "+-"
    value <- read <$> many1 digit
    return . Int $ if sign == Just '-' then negate value else value

  text = Text . Text.pack <$> (char '"' *> textContents <* char '"')
  textContents = many (noneOf "\\\"" <|> textEscape)
  textEscape = char '\\' *> choice
    [ oneOf "\\\""
    , '\a' <$ char 'a'
    , '\b' <$ char 'b'
    , '\f' <$ char 'f'
    , '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    , '\v' <$ char 'v'
    ]

  word = (alphanumeric <|> symbolic) <$$> \ name -> case name of
    "def" -> Def
    "true" -> Bool True
    "false" -> Bool False
    _ -> case Builtin.fromText nameText of
      Just builtin -> Builtin builtin
      _ -> Word nameText
      where nameText = Text.pack name
    where
    alphanumeric = (:)
      <$> (letter <|> char '_')
      <*> many (letter <|> digit <|> char '_')
    symbolic = many1 $ oneOf "!#$%&*+,-./;<=>?@^|~"

silence :: Parser ()
silence = skipMany $ comment <|> whitespace
  where
  whitespace = skipMany1 $ choice [newline, nonNewline]
  newline = do
    void $ char '\n' *> many nonNewline
    pos <- getPosition
    putState $ sourceColumn pos
  nonNewline = void $ satisfy (`elem` "\t\v\f\r ")
  comment = single <|> multi
  single = try (string "--")
    *> (anyChar `skipManyTill` (void (char '\n') <|> eof))
  multi = void $ start *> contents <* end
    where
    contents = characters *> optional multi <* characters
    characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
    start = try $ string "{-"
    end = string "-}"

skipManyTill
  :: Parser a
  -> Parser b
  -> Parser ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
