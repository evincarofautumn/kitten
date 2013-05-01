{-# LANGUAGE RecordWildCards #-}

module Kitten.Token
  ( Located(..)
  , Token(..)
  , tokenize
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Text.Parsec
  hiding ((<|>), many, newline, optional, token, tokens)

import Kitten.Location
import Kitten.Builtin (Builtin)

import qualified Kitten.Builtin as Builtin

type Parser a = ParsecT String Column Identity a

data Token
  = Arrow
  | BlockBegin
  | BlockEnd
  | Bool Bool
  | BoolType
  | Builtin Builtin
  | Def
  | Else
  | Escape
  | GroupBegin
  | GroupEnd
  | If
  | Int Int
  | IntType
  | Lambda
  | Layout
  | Text String
  | TextType
  | Then
  | VectorBegin
  | VectorEnd
  | Word String
  deriving (Eq)

instance Show Token where
  show t = case t of
    Arrow -> "->"
    BlockBegin -> "{"
    BlockEnd -> "}"
    Bool value -> if value then "true" else "false"
    BoolType -> "bool"
    Builtin name -> show name
    Def -> "def"
    Else -> "else"
    Escape -> "`"
    GroupBegin -> "("
    GroupEnd -> ")"
    If -> "if"
    Int value -> show value
    IntType -> "int"
    Lambda -> "\\"
    Layout -> ":"
    Then -> "then"
    Text value -> show value
    TextType -> "text"
    VectorBegin -> "["
    VectorEnd -> "]"
    Word word -> show word

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show (Located locatedToken _) = show locatedToken

tokenize :: String -> String -> Either ParseError [Located]
tokenize = runParser file 1

located
  :: Parser Token
  -> Parser Located
located parser = do
  indent <- getState
  start <- getPosition
  result <- parser
  return $ Located result Location
    { locationStart = start
    , locationIndent = indent
    }

file :: Parser [Located]
file = silence *> tokens <* eof

tokens :: Parser [Located]
tokens = token `sepEndBy` silence

token :: Parser Located
token = (<?> "token") . located $ choice
  [ BlockBegin <$ char '{'
  , BlockEnd <$ char '}'
  , Escape <$ char '`'
  , GroupBegin <$ char '('
  , GroupEnd <$ char ')'
  , Lambda <$ char '\\'
  , Layout <$ char ':'
  , VectorBegin <$ char '['
  , VectorEnd <$ char ']'
  , text
  , try int
  , try arrow
  , word
  ]
  where

  arrow = Arrow <$ string "->"

  int = do
    sign <- optionMaybe $ oneOf "+-"
    value <- read <$> many1 digit
    return . Int $ if sign == Just '-' then negate value else value

  text = Text <$> (char '"' *> textContents <* char '"')

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

  word = flip fmap (alphanumeric <|> symbolic) $ \ name -> case name of
    "bool" -> BoolType
    "def" -> Def
    "else" -> Else
    "false" -> Bool False
    "if" -> If
    "int" -> IntType
    "text" -> TextType
    "then" -> Then
    "true" -> Bool True
    _ -> case Builtin.fromString name of
      Just builtin -> Builtin builtin
      _ -> Word name
    where

    alphanumeric = (:)
      <$> (letter <|> char '_')
      <*> many (letter <|> digit <|> char '_')

    symbolic = many1 $ oneOf "!#$%&*+,-./;<=>?@^|~"

silence :: Parser ()
silence = skipMany $ comment <|> whitespace
  where

  whitespace = skipMany1 (newline <|> nonNewline)
    <?> "whitespace"

  newline = do
    void $ char '\n' *> many nonNewline
    pos <- getPosition
    putState $ sourceColumn pos

  nonNewline = void $ satisfy (`elem` "\t\v\f\r ")

  comment = single <|> multi
    <?> "comment"

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
