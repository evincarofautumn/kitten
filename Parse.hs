module Parse 
  ( Parse.parse
  ) where

import Program
import Term

import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad
import Data.Char
import Text.Parsec as Parsec
import Text.Parsec.String as Parsec

parse :: String -> String -> Either ParseError Program
parse = Parsec.parse programP

programP :: Parser Program
programP = Program
  <$> (optional silenceP *> many termP <* optional silenceP <* eof)

termP :: Parser Term
termP = choice
  [ quotationP
  , definitionP
  , try floatP
  , integerP
  , wordP
  , textP
  ] <* optional silenceP <?> "term"

definitionP :: Parser Term
definitionP = do
  name <- try (string "define") *> silenceP *> wordP <* optional silenceP
  term <- quotationP <|> try floatP <|> integerP <|> wordP <|> textP
  body <- case term of
    (Term.Integer _) -> return $ Term.Quotation [term]
    (Term.Float _) -> return $ Term.Quotation [term]
    (Term.Word _) -> return $ Term.Quotation [term]
    (Term.Quotation _) -> return term
    (Term.Definition _ _) -> unexpected "definition"
  return $ Term.Definition name body

quotationP :: Parser Term
quotationP = Term.Quotation <$> (left *> many termP <* right) <?> "quotation"
  where
    left = char '[' <* optional silenceP
    right = char ']' <?> "end of quotation"

wordP :: Parser Term
wordP = Term.Word <$> many1 wordCharacterP <?> "word"

floatP :: Parser Term
floatP = (Term.Float . read) <$> (bodyP <?> "float") where
  bodyP = do
    whole <- many1 digit
    separator <- char '.'
    fractional <- many1 digit
    return $ whole ++ [separator] ++ fractional

integerP :: Parser Term
integerP = (Term.Integer . read) <$> (many1 digit <?> "integer")

textP :: Parser Term
textP = Term.Quotation <$> (left *> many character <* right) <?> "string"
  where
    left = char '"'
    right = char '"' <?> "end of string"
    character = Term.Integer . fromIntegral . ord <$>
      (noneOf "\\\"" <|> escape) <?> "character or escape"
    escape = do
      c <- char '\\' *> anyChar
      case c of
        'a' -> return '\a'
        'f' -> return '\f'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        'v' -> return '\v'
        '\\' -> return '\\'
        '\"' -> return '"'
        _ -> fail $ "Invalid escape \"\\" ++ [c] ++ "\""

wordCharacterP :: Parser Char
wordCharacterP = noneOf "()[]\" \a\f\n\r\t\v"

silenceP :: Parser ()
silenceP = void . many1 $ whitespaceP <|> commentP

whitespaceP :: Parser ()
whitespaceP = (void . many1 $ satisfy isSpace) <?> "whitespace"

commentP :: Parser ()
commentP = void
  $ char '('
  *> many (try commentP <|> void (noneOf "()"))
  *> char ')'
