module Kitten (compile) where

import qualified Value
import Value (Value)
import CompileError

import Control.Applicative ((<*), (*>), (<$>))
import Control.Monad (liftM2)
import Data.Char (ord, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec as Parsec

data Program = Program [Value]

ignoreP :: Parser a -> Parser ()
ignoreP parser = parser >> return ()

silenceP :: Parser ()
silenceP = ignoreP . many1 $ whitespaceP <|> commentP

commentP :: Parser ()
commentP = ignoreP $
  ((char '(' >> (many . noneOf $ ")") >> char ')') <?> "comment")

whitespaceP :: Parser ()
whitespaceP = (ignoreP . many1 . satisfy $ isSpace) <?> "whitespace"

wordP :: Parser Value
wordP = Value.Word <$> liftM2 (:) firstP restP <?> "word"
  where
    firstP = letter <|> symbolP
    restP = many wordCharacterP

symbolP :: Parser Char
symbolP = oneOf "!#$%&*+-./:;<=>?@^_|~"

wordCharacterP :: Parser Char
wordCharacterP = letter <|> digit <|> symbolP

integerP :: Parser Value
integerP = (Value.Integer . read) <$> (many1 digit <?> "integer")

floatP :: Parser Value
floatP = (Value.Float . read) <$> (bodyP <?> "float") where
  bodyP = do
    whole <- many1 digit
    separator <- char '.'
    fractional <- many1 digit
    return $ whole ++ [separator] ++ fractional

quotationP :: Parser Value
quotationP = Value.Quotation <$> (left *> many termP <* right) <?> "quotation"
  where
    left  = char '[' <* optional silenceP
    right = char ']' <?> "end of quotation"

textP :: Parser Value
textP = Value.Quotation <$> (left *> many character <* right) <?> "string"
  where
    left      = char '"'
    right     = char '"' <?> "end of string"
    character = Value.Integer . fromIntegral . ord <$>
      (noneOf "\\\"" <|> (char '\\' *> (char '\\' <|> char '\"')))
      <?> "character or escape"

definitionP :: Parser Value
definitionP = do
  name <- (try $ string "define") *> silenceP *> wordP <* optional silenceP
  term <- quotationP <|> wordP <|> try floatP <|> integerP <|> textP
  body <- case term of
    (Value.Word _)         -> return $ Value.Quotation [term]
    (Value.Integer _)      -> return $ Value.Quotation [term]
    (Value.Float _)        -> return $ Value.Quotation [term]
    (Value.Quotation _)    -> return term
    (Value.Definition _ _) -> unexpected "definition"
  return $ Value.Definition name body

termP :: Parser Value
termP = ((quotationP <|> definitionP <|> wordP <|> try floatP
  <|> integerP <|> textP) <* optional silenceP) <?> "term"

programP :: Parser Program
programP = Program <$>
  (optional silenceP *> many termP <* optional silenceP <* eof)

parse :: String -> Either ParseError Program
parse = Parsec.parse programP []

compile :: String -> Either CompileError String
compile source = case Kitten.parse source of
  Left parseError -> Left . CompileError . show $ parseError
  Right (Program parseResult) -> case Value.compile parseResult of
    Right compiledProgram -> Right
      . ("#include \"kitten.h\"\nKITTEN_PROGRAM(" ++) . (++ ")")
      . intercalate " " $ compiledProgram
    Left compileError -> Left compileError
