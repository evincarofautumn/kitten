module Kitten (compile) where

import qualified Value
import Value (Value, compileValue)
import Control.Applicative ((<*), (*>), (<$>))
import Control.Monad (liftM2)
import Data.Char (ord, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Program = Program [Value]

data CompileError = CompileError String

instance Show CompileError where
  show (CompileError message) = message

ignoreP :: Parser a -> Parser ()
ignoreP parser = parser >> return ()

spaces :: Parser ()
spaces = ignoreP . many $ (ignoreP . many1 . satisfy $ isSpace) <|> commentP
  where
    commentP = ignoreP $ ((char '(' >> (many . noneOf $ ")") >> char ')')
      <?> "comment")

wordP :: Parser Value
wordP = Value.Word <$> (liftM2 (:) firstP restP) <?> "word"
  where
    firstP = letter <|> char '_'
    restP = many $ letter <|> digit <|> char '_'

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
quotationP = Value.Quotation <$> (left *> many termP <* right)
  where
    left  = char '[' <* spaces
    right = char ']' <?> "end of quotation"

textP :: Parser Value
textP = Value.Quotation <$> (left *> many character <* right)
  where
    left      = char '"'
    right     = char '"' <?> "end of string"
    character = Value.Integer . fromIntegral . ord <$>
      (noneOf "\\\"" <|> (char '\\' *> (char '\\' <|> char '\"')))
      <?> "character or escape"

termP :: Parser Value
termP = ((quotationP <|> wordP <|> try floatP <|> integerP <|> textP)
  <* spaces) <?> "term"

programP :: Parser Program
programP = Program <$> (spaces *> many termP <* spaces <* eof)

parse :: String -> Either ParseError Program
parse = Parsec.parse programP []

compile :: String -> Either CompileError String
compile source = case Kitten.parse source of
  Left parseError -> Left . CompileError . show $ parseError
  Right (Program parseResult) -> Right
    . ("#include \"kitten.h\"\nKITTEN_PROGRAM(" ++) . (++ ")")
    . intercalate " " . map compileValue $ parseResult
