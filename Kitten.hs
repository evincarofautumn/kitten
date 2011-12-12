module Kitten (compile) where

import qualified Value
import Value (Value)
import Control.Applicative ((<*), (*>), (<$>))
import Control.Arrow ((>>>))
import Control.Monad (liftM2)
import Data.Char (isSpace, isAlphaNum)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec as Parsec

data Program = Program [Value]
instance Show Program where
  show (Program terms) =
    map show >>> intercalate " " $ terms

data CompileError = CompileError String deriving (Show)

word :: Parser Value
word = Value.Word <$> (normal <|> symbolic) <?> "word"
  where
    normal = liftM2 (:) first rest
    first = letter <|> char '_'
    rest = many $ letter <|> digit <|> char '_'
    symbolic = many1 . satisfy $ \c ->
      (c /= '[') && (c /= ']') && (c /= '_')
        && not (isSpace c) && not (isAlphaNum c)

integer :: Parser Value
integer = (read >>> Value.Integer) <$> (many1 digit <?> "integer")

float :: Parser Value
float = (read >>> Value.Float) <$> (many1 $ digit <|> char '_') <?> "float"

quotation :: Parser Value
quotation = Value.Quotation <$> (left *> many term <* right)
  where
   left  = char '[' <* spaces
   right = (char ']' <* spaces) <?> "end of quotation"

term :: Parser Value
term = ((quotation <|> word <|> try float <|> integer) <* spaces) <?> "term"

program :: Parser Program
program = Program <$> many term <* eof

parse :: String -> Either ParseError Program
parse = Parsec.parse program []

compile :: String -> Either CompileError String
compile source = case Kitten.parse source of
  Left parseError -> Left . CompileError . show $ parseError
  Right parseResult -> Right . show $ parseResult
