{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Data.Function
import System.IO

import qualified Text.Parsec as P

main :: IO ()
main = fix $ \ loop -> do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      putStrLn $ case compile "STDIN" line of
        Left compileError -> show compileError
        Right compileResult -> unwords $ map show compileResult
      loop

data CompileError
  = CompileError String

instance Show CompileError where
  show (CompileError message) = message

data Token
  = Word String
  | Def
  | Lambda
  | VecBegin
  | VecEnd
  | FunBegin
  | FunEnd

instance Show Token where
  show (Word word) = word
  show Def = "def"
  show Lambda = "\\"
  show VecBegin = "("
  show VecEnd = ")"
  show FunBegin = "["
  show FunEnd = "]"

compile :: String -> String -> Either CompileError [Token]
compile name source
  = case tokenize name source of
    Left error -> Left $ CompileError (show error)
    Right result -> Right result

tokenize :: String -> String -> Either P.ParseError [Token]
tokenize name source = P.parse file name source

file :: (P.Stream s m Char) => P.ParsecT s u m [Token]
file = tokens <* P.eof

tokens :: (P.Stream s m Char) => P.ParsecT s u m [Token]
tokens = token `P.sepBy` P.spaces

token :: (P.Stream s m Char) => P.ParsecT s u m Token
token = P.choice [lambda, vecBegin, vecEnd, funBegin, funEnd, word]
  where
  lambda = Lambda <$ P.char '\\'
  vecBegin = VecBegin <$ P.char '('
  vecEnd = VecEnd <$ P.char ')'
  funBegin = FunBegin <$ P.char '['
  funEnd = FunEnd <$ P.char ']'
  word = do
    word <- P.many1 (P.letter <|> P.digit <|> P.char '_')
    case word of
      "def" -> return Def
      _ -> return $ Word word
