{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Tokenize
  ( tokenize
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import Numeric
import Text.Parsec.Text ()

import qualified Data.Text as T
import qualified Text.Parsec as Parsec

import Kitten.Parsec
import Kitten.Location
import Kitten.Token
import Kitten.Util.Applicative
import Kitten.Util.Parsec

import qualified Kitten.Builtin as Builtin

type Parser a = ParsecT Text Column Identity a

tokenize :: String -> Text -> Either ParseError [Located]
tokenize = runParser file 1

located
  :: Parser Token
  -> Parser Located
located parser = do
  indent <- getState
  start <- getPosition
  result <- parser
  return $ Located result Location
    { locationStart = start
    , locationIndent = indent
    }

file :: Parser [Located]
file = silence *> tokens <* eof

tokens :: Parser [Located]
tokens = token `sepEndBy` silence

token :: Parser Located
token = (<?> "token") . located $ choice
  [ BlockBegin NormalBlockHint <$ char '{'
  , BlockEnd <$ char '}'
  , Char <$> (char '\'' *> character '\'' <* char '\'')
  , Comma <$ char ','
  , GroupBegin <$ char '('
  , GroupEnd <$ char ')'
  , Layout <$ char ':'
  , VectorBegin <$ char '['
  , VectorEnd <$ char ']'
  , Text <$> between (char '"') (char '"') text
  , try number
  , try $ Arrow <$ (string "->" <|> string "\x2192")
    <* notFollowedBy symbolCharacter
  , word
  ]
  where

  number :: Parser Token
  number = do
    sign <- optionMaybe $ oneOf "+-"
    let
      applySign :: (Num a) => a -> a
      applySign = if sign == Just '-' then negate else id

    choice
      [ try . fmap (\ (hint, value) -> Int (applySign value) hint)
        $ char '0' *> choice
        [ base 'b' "01" readBin BinaryHint "binary"
        , base 'o' ['0'..'7'] readOct' OctalHint "octal"
        , base 'x' (['0'..'9'] ++ ['A'..'F']) readHex'
          HexadecimalHint "hexadecimal"
        ]
      , do
        integer <- many1 digit
        mFraction <- optionMaybe $ (:) <$> char '.' <*> many1 digit
        return $ case mFraction of
          Just fraction -> Float . applySign . read $ integer ++ fraction
          Nothing -> Int (applySign $ read integer) DecimalHint
      ]

  base prefix digits readBase hint desc = fmap ((,) hint . readBase)
    $ char prefix *> many1 (oneOf digits <?> (desc ++ " digit"))

  text :: Parser Text
  text = T.pack <$> many (character '"')

  character :: Char -> Parser Char
  character quote
    = (noneOf ('\\' : [quote]) <?> "character") <|> escape

  escape :: Parser Char
  escape = (<?> "escape") $ char '\\' *> choice
    [ oneOf "\\\"'"
    , '\a' <$ char 'a'
    , '\b' <$ char 'b'
    , '\f' <$ char 'f'
    , '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    , '\v' <$ char 'v'
    ]

  word :: Parser Token
  word = choice
    [ ffor alphanumeric $ \ name -> case name of
      "Bool" -> BoolType
      "Char" -> CharType
      "Float" -> FloatType
      "Handle" -> HandleType
      "IO" -> IOType
      "Int" -> IntType
      "choice" -> Choice
      "def" -> Def
      "else" -> Else
      "false" -> Bool False
      "from" -> From
      "if" -> If
      "import" -> Import
      "option" -> Option
      "to" -> To
      "true" -> Bool True
      "type" -> Type
      (T.unpack -> first : _) | isUpper first -> BigWord name
      _ -> case Builtin.fromText name of
        Just builtin -> Builtin builtin
        _ -> LittleWord name
    , ffor symbolic $ \ name -> case Builtin.fromText name of
      Just builtin -> Builtin builtin
      _ -> Operator name
    ]
    where

    alphanumeric :: Parser Text
    alphanumeric
      = (\ x y z -> T.pack $ x : y ++ maybeToList z)
      <$> (Parsec.satisfy isLetter <|> char '_')
      <*> (many . choice)
        [ Parsec.satisfy isLetter
        , Parsec.satisfy isDigit
        , char '_'
        ]
      <*> optionMaybe (oneOf "'\x2032\x2033\x2034\x2057")

    symbolic :: Parser Text
    symbolic = T.pack <$> many1 symbolCharacter

  symbolCharacter :: Parser Char
  symbolCharacter = notFollowedBy special
    *> (Parsec.satisfy isSymbol <|> Parsec.satisfy isPunctuation)

  special :: Parser Char
  special = oneOf "\"'(),:[]_{}"

readBin :: String -> Int
readBin = go 0
  where
  go !acc ('0':xs) = go (2 * acc + 0) xs
  go !acc ('1':xs) = go (2 * acc + 1) xs
  go !acc [] = acc
  go _ (_:_) = error "Kitten.Tokenize.readBin: non-binary digit"

readHex', readOct' :: String -> Int
readHex' = fst . head . readHex
readOct' = fst . head . readOct

silence :: Parser ()
silence = skipMany $ comment <|> whitespace
  where

  whitespace = skipMany1 (newline <|> nonNewline)
    <?> "whitespace"

  newline = do
    void $ char '\n' *> many nonNewline
    pos <- getPosition
    putState $ sourceColumn pos

  nonNewline = void $ Parsec.satisfy (`elem` "\t\v\f\r ")

  comment = single <|> multi <?> "comment"

  single = try (string "//")
    *> (anyChar `skipManyTill` (newline <|> eof))

  multi = void $ start *> contents <* end
    where
    contents = characters *> optional multi <* characters
    characters = skipMany $ notFollowedBy (start <|> end) *> anyChar
    start = try $ string "/*"
    end = string "*/"
