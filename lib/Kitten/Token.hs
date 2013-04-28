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
  = Word String
  | Builtin Builtin
  | Int Int
  | IntType
  | Bool Bool
  | BoolType
  | Text String
  | TextType
  | Arrow
  | Def
  | Type
  | Lambda
  | VectorBegin
  | VectorEnd
  | BlockBegin
  | BlockEnd
  | GroupBegin
  | GroupEnd
  | Layout
  deriving (Eq)

instance Show Token where
  show t = case t of
    Word word -> show word
    Builtin name -> show name
    Int value -> show value
    IntType -> "int"
    Bool value -> if value then "true" else "false"
    BoolType -> "bool"
    Text value -> show value
    TextType -> "text"
    Arrow -> "->"
    Def -> "def"
    Type -> "type"
    Lambda -> "\\"
    VectorBegin -> "["
    VectorEnd -> "]"
    BlockBegin -> "{"
    BlockEnd -> "}"
    GroupBegin -> "("
    GroupEnd -> ")"
    Layout -> ":"

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
  end <- getPosition
  return $ Located result Location
    { locationStart = start
    , locationEnd = end
    , locationIndent = indent
    }

file :: Parser [Located]
file = silence *> tokens <* eof

tokens :: Parser [Located]
tokens = token `sepEndBy` silence

token :: Parser Located
token = (<?> "token") . located $ choice
  [ Lambda <$ char '\\'
  , VectorBegin <$ char '['
  , VectorEnd <$ char ']'
  , BlockBegin <$ char '{'
  , BlockEnd <$ char '}'
  , GroupBegin <$ char '('
  , GroupEnd <$ char ')'
  , Layout <$ char ':'
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
    "int" -> IntType
    "text" -> TextType
    "def" -> Def
    "type" -> Type
    "true" -> Bool True
    "false" -> Bool False
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
