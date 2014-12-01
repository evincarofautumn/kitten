{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

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
import Text.Parsec.Pos
import Text.Parsec.Text ()

import qualified Data.Text as T
import qualified Text.Parsec as Parsec

import Kitten.Location
import Kitten.Name
import Kitten.Parsec
import Kitten.Token
import Kitten.Util.Applicative
import Kitten.Util.Function
import Kitten.Util.Parsec

type Parser a = ParsecT Text Column Identity a

tokenize :: Int -> String -> Text -> Either ParseError [Located]
tokenize firstLine name source = runParser parser 1 name source
  where parser = setPosition (newPos name firstLine 1) >> file

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
  [ TkBlockBegin NormalBlockHint <$ char '{'
  , TkBlockEnd <$ char '}'
  , do
    mc <- char '\'' *> character '\'' <* char '\''
    case mc of
      Just c -> return $ TkChar c
      Nothing -> unexpected "empty character literal"
  , TkComma <$ char ','
  , try $ TkEllipsis <$ (string "..." <|> string "\x2026")
  , TkGroupBegin <$ char '('
  , TkGroupEnd <$ char ')'
  , TkIgnore <$ try (char '_' <* notFollowedBy letterCharacter)
  , TkVocabLookup <$ try (string "::")
  , TkLayout <$ char ':'
  , TkVectorBegin <$ char '['
  , TkVectorEnd <$ char ']'
  , TkReference <$ char '\\'
  , TkText <$> between (char '"') (char '"') text
  , try number
  , try $ TkArrow <$ (string "->" <|> string "\x2192")
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
      [ try . fmap (\(hint, value) -> TkInt (applySign value) hint)
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
          Just fraction -> TkFloat . applySign . read $ integer ++ fraction
          Nothing -> TkInt (applySign $ read integer) DecimalHint
      ]

  base prefix digits readBase hint desc = fmap ((,) hint . readBase)
    $ char prefix *> many1 (oneOf digits <?> (desc ++ " digit"))

  text :: Parser Text
  text = T.pack . catMaybes <$> many (character '"')

  character :: Char -> Parser (Maybe Char)
  character quote
    = (Just <$> noneOf ('\\' : [quote]) <?> "character") <|> escape

  escape :: Parser (Maybe Char)
  escape = (<?> "escape") $ char '\\' *> choice
    [ Just <$> oneOf "\\\"'"
    , Just '\a' <$ char 'a'
    , Just '\b' <$ char 'b'
    , Just '\f' <$ char 'f'
    , Just '\n' <$ char 'n'
    , Just '\r' <$ char 'r'
    , Just '\t' <$ char 't'
    , Just '\v' <$ char 'v'
    , Just . head <$> many1 space
    , Nothing <$ char '&'
    ]

  word :: Parser Token
  word = choice
    [ ffor alphanumeric $ \name -> case name of
      "abbrev" -> TkAbbrev
      "case" -> TkCase
      "data" -> TkData
      "default" -> TkDefault
      "define" -> TkDefine
      "false" -> TkBool False
      "infix" -> TkInfix
      "import" -> TkImport
      "match" -> TkMatch
      "true" -> TkBool True
      "vocab" -> TkVocab
      _ -> TkWord (Unqualified name)
    , ffor symbolic $ TkOperator . Unqualified
    ]
    where

    alphanumeric :: Parser Text
    alphanumeric = T.pack .: (:)
      <$> (letterCharacter <|> char '_')
      <*> (many . choice) [letterCharacter, char '_', digit]

    symbolic :: Parser Text
    symbolic = T.pack <$> many1 symbolCharacter

  letterCharacter :: Parser Char
  letterCharacter = Parsec.satisfy isLetter

  symbolCharacter :: Parser Char
  symbolCharacter = notFollowedBy special
    *> (Parsec.satisfy isSymbol <|> Parsec.satisfy isPunctuation)

  special :: Parser Char
  special = oneOf "\"'(),:[\\]_{}"

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
