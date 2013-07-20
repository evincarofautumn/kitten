module Kitten.Tokenize
  ( tokenize
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Identity

import qualified Text.Parsec as Parsec

import Kitten.Parsec
import Kitten.Location
import Kitten.Token
import Kitten.Util.Applicative
import Kitten.Util.Parsec

import qualified Kitten.Builtin as Builtin

type Parser a = ParsecT String Column Identity a

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
  , Char <$> (char '\'' *> character '\'' <* char '\'')
  , Comma <$ char ','
  , GroupBegin <$ char '('
  , GroupEnd <$ char ')'
  , Layout <$ char ':'
  , VectorBegin <$ char '['
  , VectorEnd <$ char ']'
  , Text <$> (char '"' *> text <* char '"')
  , try number
  , try $ Arrow <$ string "->" <* notFollowedBy symbolCharacter
  , word
  ]
  where

  number :: Parser Token
  number = do
    sign <- optionMaybe $ oneOf "+-"
    let
      applySign :: (Num a) => a -> a
      applySign = if sign == Just '-' then negate else id
    integer <- many1 digit
    mFraction <- optionMaybe $ (:) <$> char '.' <*> many1 digit
    return $ case mFraction of
      Just fraction -> Float . applySign . read $ integer ++ fraction
      Nothing -> Int . applySign $ read integer

  text :: Parser String
  text = many $ character '"'

  character :: Char -> Parser Char
  character quote = noneOf ('\\' : [quote]) <|> escape

  escape :: Parser Char
  escape = char '\\' *> choice
    [ oneOf "\\\"'"
    , '\a' <$ char 'a'
    , '\b' <$ char 'b'
    , '\f' <$ char 'f'
    , '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    , '\v' <$ char 'v'
    ]

  word :: Parser Token
  word = choice
    [ ffor alphanumeric $ \ name -> case name of
      "Bool" -> BoolType
      "Char" -> CharType
      "Float" -> FloatType
      "Handle" -> HandleType
      "IO" -> IOType
      "Int" -> IntType
      "def" -> Def
      "else" -> Else
      "false" -> Bool False
      "if" -> If
      "then" -> Then
      "true" -> Bool True
      (first : _) | isUpper first -> BigWord name
      _ -> case Builtin.fromString name of
        Just builtin -> Builtin builtin
        _ -> LittleWord name
    , ffor symbolic $ \ name -> case Builtin.fromString name of
      Just builtin -> Builtin builtin
      _ -> Operator name
    ]
    where

    alphanumeric :: Parser String
    alphanumeric = (:)
      <$> (letter <|> char '_')
      <*> many (letter <|> digit <|> char '_')

    symbolic :: Parser String
    symbolic = many1 symbolCharacter

  symbolCharacter :: Parser Char
  symbolCharacter = oneOf "!#$%&*+-./;<=>?@\\^|~"

silence :: Parser ()
silence = skipMany $ comment <|> whitespace
  where

  whitespace = skipMany1 (newline <|> nonNewline)
    <?> "whitespace"

  newline = do
    void $ char '\n' *> many nonNewline
    pos <- getPosition
    putState $ sourceColumn pos

  nonNewline = void $ Parsec.satisfy (`elem` "\t\v\f\r ")

  comment = single <|> multi <?> "comment"

  single = try (string "//")
    *> (anyChar `skipManyTill` (void (char '\n') <|> eof))

  multi = void $ start *> contents <* end
    where
    contents = characters *> optional multi <* characters
    characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
    start = try $ string "/*"
    end = string "*/"
