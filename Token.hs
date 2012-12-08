{-# LANGUAGE FlexibleContexts #-}

module Token
  ( Located(..)
  , Token(..)
  , tokenize
  ) where

import Control.Applicative
import Control.Monad.Identity

import qualified Text.Parsec as P

type Parser a = P.ParsecT String P.Column Identity a

data Token
  = Word String
  | Int Integer
  | Def
  | Lambda
  | VecBegin
  | VecEnd
  | FunBegin
  | FunEnd
  deriving (Eq)

instance Show Token where
  show (Word word) = word
  show (Int value) = show value
  show Def = "def"
  show Lambda = "\\"
  show VecBegin = "("
  show VecEnd = ")"
  show FunBegin = "["
  show FunEnd = "]"

data Located = Located
  { locatedLocation :: P.SourcePos
  , locatedIndent :: P.Column
  , locatedToken :: Token
  }

instance Show Located where
  show (Located _ _ token) = show token

tokenize :: String -> String -> Either P.ParseError [Located]
tokenize name source = P.runParser file 0 name source

located
  :: Parser Token.Token
  -> Parser Token.Located
located parser = do
  indent <- P.getState
  position <- P.getPosition
  result <- parser
  return $ Token.Located position indent result

file :: Parser [Located]
file = tokens <* P.eof

tokens :: Parser [Located]
tokens = token `P.sepBy` P.spaces

token :: Parser Located
token = located $ P.choice
  [ lambda
  , vecBegin
  , vecEnd
  , funBegin
  , funEnd
  , int
  , word
  ]
  where
  lambda = Lambda <$ P.char '\\'
  vecBegin = VecBegin <$ P.char '('
  vecEnd = VecEnd <$ P.char ')'
  funBegin = FunBegin <$ P.char '['
  funEnd = FunEnd <$ P.char ']'
  int = Int . read <$> P.many1 P.digit
  word = do
    word <- P.many1 (P.letter <|> P.digit <|> P.char '_')
    case word of
      "def" -> return Def
      _ -> return $ Word word
