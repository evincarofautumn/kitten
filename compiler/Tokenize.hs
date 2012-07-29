{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenize
  ( tokenize
  ) where

import Error
import Text ((<++>), toText)
import qualified Text
import qualified Token
import Utils

import Control.Applicative (Applicative, (<$>), (<*), (<*>), (*>))
import Control.Monad
import Control.Monad.Identity
import Text.Parsec as Parsec hiding (newline, token, tokens)

type TokenParser a = ParsecT String Column Identity a

tokenize
  :: String
  -> String
  -> Either ParseError [Token.Located]
tokenize = Parsec.runParser tokens 0

tokens :: TokenParser [Token.Located]
tokens = optional whitespace *> many (token <* whitespace) <* eof

token :: TokenParser Token.Located
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
  :: TokenParser Token.Token
  -> TokenParser Token.Located
located parser = do
  indent <- getState
  position <- getPosition
  result <- parser
  return $ Token.Located position indent result

whitespace :: TokenParser ()
whitespace = skipMany $ comment <|> literalWhitespace
  where
    literalWhitespace = skipMany1 $ choice [newline, nonNewline]
    newline = do
      void $ char '\n' *> many nonNewline
      pos <- getPosition
      putState $ sourceColumn pos

    nonNewline        = void $ satisfy (`elem` "\t\v\f\r ")
    comment           = singleLineComment <|> multiLineComment
    singleLineComment = string "--" *> (anyChar `skipManyTill` char '\n')
    multiLineComment  = void $ start *> contents <* end
      where
        contents   = characters *> optional multiLineComment <* characters
        characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
        start      = string "{-"
        end        = string "-}"

quotationOpen :: TokenParser Token.Token
quotationOpen = char '[' *> return Token.QuotationOpen

quotationClose :: TokenParser Token.Token
quotationClose = char ']' *> return Token.QuotationClose

definition :: TokenParser Token.Token
definition = string "=>" *> return Token.Definition

layout :: TokenParser Token.Token
layout = char ':' *> return Token.Layout

textQuotation :: TokenParser Token.Token
textQuotation = do
  quote <- open
  text  <- body quote
  void (close quote) <?> "end of text quotation"
  return $ Token.Text text
  where
    open = oneOf "\"“\'‘"
    close = char . matching

    matching '\"' = '\"'
    matching '\'' = '\''
    matching '“'  = '”'
    matching '‘'  = '’'
    matching _    = $(impossible)

    body quote
      | isNestable quote
      = Text.concat <$> (many . choice)
        [ char quote <++> body quote <++> close quote
        , toText <$> noneOf (quote : matching quote : "\\")
        , escape
        ]
    body quote = Text.concat
      <$> (many . choice)
      [ toText <$> noneOf (quote : "\\")
      , escape
      ]

    isNestable c = c `elem` "“‘"

    escape :: TokenParser Text.Text
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

word :: TokenParser Token.Token
word = Token.Word <$> word' <?> "word"

symbol :: TokenParser Token.Token
symbol = Token.Symbol <$> (char '.' *> word')

word' :: TokenParser Text.Text
word' = Text.pack <$> many1 wordCharacter 
  where wordCharacter = noneOf "\a\t\n\v\f\r \"\'():[]{}‘’“”"

number :: TokenParser Token.Token
number = (<?> "number") $ do
  start        <- many1 digit
  maybeInexact <- optionMaybe $ (:) <$> char '.' <*> many1 digit
  return $ case maybeInexact of
    Just inexact -> Token.Inexact . read $ start ++ inexact
    Nothing      -> Token.Integer $ read start
