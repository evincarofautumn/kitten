{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenize
  ( tokenize
  ) where

import qualified Token

import Control.Applicative (Applicative, (<$>), (<*), (<*>), (*>))
import Control.Monad
import Data.Char
import Text ((<++>), toText)
import qualified Text as Text
import Text.Parsec as Parsec hiding (token, tokens)
import Text.Parsec.String as Parsec

tokenize :: String -> String -> Either ParseError [Token.Located]
tokenize = Parsec.parse tokens

tokens :: Parser [Token.Located]
tokens = optional whitespace *> many (token <* whitespace) <* eof

token :: Parser Token.Located
token = located $ choice
  [ quotationOpen
  , quotationClose
  , word
  , definition
--, layout
  , textQuotation
  , symbol
  , integer
--, rational
--, inexact
  ]

whitespace = skipMany $ comment <|> literalWhitespace

literalWhitespace = void . many1 $ satisfy isSpace

comment = singleLineComment <|> multiLineComment

singleLineComment = string "--" *> (anyChar `skipManyTill` char '\n')

multiLineComment = void $ start *> contents <* end
  where
    contents = characters *> optional multiLineComment <* characters
    characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
    start = string "{-"
    end = string "-}"

skipManyTill
  :: Parser a
  -> Parser b
  -> Parser ()
a `skipManyTill` b = void (try b) <|> (a *> (a `skipManyTill` b))

located
  :: (Monad m)
  => ParsecT s u m Token.Token
  -> ParsecT s u m Token.Located
located parser = Token.Located <$> getPosition <*> parser

quotationOpen = char '[' *> return Token.QuotationOpen

quotationClose = char ']' *> return Token.QuotationClose

definition = string "=>" *> return Token.Definition

layout :: (Monad m, Stream s m Char) => ParsecT s u m Token.Token
layout = char ':' *> return Token.Layout

textQuotation = do
  quote <- open
  text <- body quote
  end <- close quote <?> "end of text quotation"
  return $ Token.Text text
  where
    open = oneOf "\"“\'‘"
    close '\"' = char '\"'
    close '\'' = char '\''
    close '“' = char '”'
    close '‘' = char '’'
    close _ = undefined
    body :: Char -> Parser Text.Text
    body quote
      | isNestable quote
      = Text.concat <$> (many . choice)
        [ toText <$> noneOf (quote : "\\")
        , char quote <++> body quote <++> close quote
        , escape
        ]
    body quote = Text.concat
      <$> (many . choice)
      [ toText <$> noneOf (quote : "\\")
      , escape
      ]
    isNestable c = c `elem` "“‘"
    escape :: Parser Text.Text
    escape = (<?> "escape") $ do 
      c <- char '\\' *> anyChar
      case c of
        'a' -> return "\a"
        'b' -> return "\b"
        'e' -> return "\ESC"
        'f' -> return "\f"
        'n' -> return "\n"
        'r' -> return "\r"
        't' -> return "\t"
        'v' -> return "\v"
        '\\' -> return "\\"
        '\"' -> return "\""
        '\'' -> return "\'"
        _ -> fail $ "Invalid escape \"\\" ++ [c] ++ "\""

word = Token.Word <$> word'
word' = letter <++> many1 (letter <|> digit)
--   where
--     symbol = noneOf "\"“”\'‘’[](){}\\#."

symbol = Token.Symbol <$> (char '.' *> word')

integer = Token.Integer . read <$> many1 digit

-- rational
-- inexact
