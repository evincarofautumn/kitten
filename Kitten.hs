module Kitten (compile) where

import qualified Value
import Value (Value, compileValue)
import Control.Applicative ((<*), (*>), (<$>))
import Control.Arrow ((>>>))
import Control.Monad (liftM2)
import Data.Char (ord, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Program = Program [Value]

data CompileError = CompileError String deriving (Show)

spaces :: Parser ()
spaces = (((many1 . satisfy $ isSpace) >> return ()) <|> comment) >> return ()
  where
    comment = char '(' >> (many1 $ noneOf ")") >> char ')' >> return ()

word :: Parser Value
word = Value.Word <$> (liftM2 (:) first rest) <?> "word"
  where
    first = letter <|> char '_'
    rest = many $ letter <|> digit <|> char '_'

integer :: Parser Value
integer = (read >>> Value.Integer) <$> (many1 digit <?> "integer")

float :: Parser Value
float = (read >>> Value.Float) <$> (do { whole <- many1 digit;
  separator <- char '.'; fractional <- many1 digit; return $ whole
  ++ [separator] ++ fractional }) <?> "float"

quotation :: Parser Value
quotation = Value.Quotation <$> (left *> many term <* right)
  where
    left  = char '[' <* spaces
    right = (char ']' <* spaces) <?> "end of quotation"

text :: Parser Value
text = Value.Quotation <$> (left *> many character <* right)
  where
    left      = char '"'
    right     = char '"' <?> "end of string"
    character = Value.Integer . fromIntegral . ord <$>
      (noneOf "\\\"" <|> (char '\\' *> (char '\\' <|> char '\"')))

term :: Parser Value
term = ((quotation <|> word <|> try float <|> integer <|> text) <* spaces)
  <?> "term"

program :: Parser Program
program = Program <$> many term <* eof

parse :: String -> Either ParseError Program
parse = Parsec.parse program []

compile :: String -> Either CompileError String
compile source = case Kitten.parse source of
  Left parseError -> (show >>> CompileError >>> Left) parseError
  Right (Program parseResult) ->
    (map compileValue >>> intercalate " "
      >>> ("#include \"kitten.h\"\nKITTEN_PROGRAM(" ++) >>> (++ ")")
      >>> Right) parseResult
