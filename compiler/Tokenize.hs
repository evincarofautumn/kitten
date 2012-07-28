{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenize
  ( tokenize
  ) where

import Text ((<++>), toText)
import qualified Text
import qualified Token
import Utils

import Control.Applicative (Applicative, (<$>), (<*), (<*>), (*>))
import Control.Monad
import Data.Char
import Text.Parsec as Parsec hiding (token, tokens)
import Text.Parsec.String as Parsec

tokenize
  :: String
  -> String
  -> Either ParseError [Token.Located]
tokenize = Parsec.parse tokens

tokens :: Parser [Token.Located]
tokens = optional whitespace *> many (token <* whitespace) <* eof

token :: Parser Token.Located
token = located $ choice
  [ quotationOpen
  , quotationClose
  , definition  
  , layout
  , textQuotation
  , number
  , word
  , symbol
  ]

located
  :: Parser Token.Token
  -> Parser Token.Located
located parser = Token.Located <$> getPosition <*> parser

whitespace :: Parser ()
whitespace = skipMany $ comment <|> literalWhitespace
  where
    literalWhitespace = void . many1 $ satisfy isSpace
    comment           = singleLineComment <|> multiLineComment
    singleLineComment = string "--" *> (anyChar `skipManyTill` char '\n')
    multiLineComment  = void $ start *> contents <* end
      where
        contents   = characters *> optional multiLineComment <* characters
        characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
        start      = string "{-"
        end        = string "-}"

quotationOpen :: Parser Token.Token
quotationOpen = char '[' *> return Token.QuotationOpen

quotationClose :: Parser Token.Token
quotationClose = char ']' *> return Token.QuotationClose

definition :: Parser Token.Token
definition = string "=>" *> return Token.Definition

layout :: Parser Token.Token
layout = char ':' *> return Token.Layout

textQuotation :: Parser Token.Token
textQuotation = do
  quote <- open
  text  <- body quote
  void (close quote) <?> "end of text quotation"
  return $ Token.Text text
  where
    open = oneOf "\"“\'‘"

    close '\"' = char '\"'
    close '\'' = char '\''
    close '“'  = char '”'
    close '‘'  = char '’'
    close _    = undefined

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

    escape = (<?> "escape") $ do 
      c <- char '\\' *> anyChar
      case c of
        'a'  -> return "\a"
        'b'  -> return "\b"
        'e'  -> return "\ESC"
        'f'  -> return "\f"
        'n'  -> return "\n"
        'r'  -> return "\r"
        't'  -> return "\t"
        'v'  -> return "\v"
        '\\' -> return "\\"
        '\"' -> return "\""
        '\'' -> return "\'"
        _    -> fail $ "Invalid escape \"\\" ++ [c] ++ "\""

word :: Parser Token.Token
word = Token.Word <$> word' <?> "word"

symbol :: Parser Token.Token
symbol = Token.Symbol <$> (char '.' *> word')

word' :: Parser Text.Text
word' = Text.pack <$> many1 wordCharacter 
  where wordCharacter = noneOf "\a\t\n\v\f\r \"\'():[]{}‘’“”"

number :: Parser Token.Token
number = (<?> "number") $ do
  start        <- many1 digit
  maybeInexact <- optionMaybe $ (:) <$> char '.' <*> many1 digit
  return $ case maybeInexact of
    Just inexact -> Token.Inexact . read $ start ++ inexact
    Nothing      -> Token.Integer $ read start
