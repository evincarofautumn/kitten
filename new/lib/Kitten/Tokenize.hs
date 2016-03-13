{-# LANGUAGE OverloadedStrings #-}

module Kitten.Tokenize
  ( tokenize
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isLetter, isPunctuation, isSymbol)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Kitten.Base (Base(..))
import Kitten.Indent (Indent(..))
import Kitten.Informer (Informer(..))
import Kitten.Located (Located(..))
import Kitten.Name (Unqualified(..))
import Kitten.Token (Token(..))
import Numeric
import Text.Parsec ((<?>), Column, ParsecT)
import qualified Data.Text as Text
import qualified Kitten.Layoutness as Layoutness
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Text.Parsec as Parsec

tokenize :: (Informer m) => FilePath -> Text -> m [Located Token]
tokenize path text = case Parsec.runParser fileTokenizer 1 path text of
  Left parseError -> do
    report $ Report.parseError parseError
    halt
  Right result -> return result

type Tokenizer a = ParsecT Text Column Identity a

fileTokenizer :: Tokenizer [Located Token]
fileTokenizer = silenceTokenizer *> tokensTokenizer <* Parsec.eof

silenceTokenizer :: Tokenizer ()
silenceTokenizer = Parsec.skipMany (comment <|> whitespace)
  where
  whitespace = Parsec.skipMany1 (newline <|> nonNewline) <?> "whitespace"

  newline = do
    void (Parsec.char '\n' *> many nonNewline)
    pos <- Parsec.getPosition
    Parsec.putState (Parsec.sourceColumn pos)

  nonNewline = void (Parsec.satisfy (`elem` ("\t\v\f\r " :: String)))

  comment = single <|> multi <?> "comment"

  single = Parsec.try (Parsec.string "//")
    *> (Parsec.anyChar `skipManyTill` (newline <|> Parsec.eof))

  multi = void (Parsec.between start end contents)
    where
    contents = characters *> optional multi <* characters
    characters = Parsec.skipMany
      $ Parsec.notFollowedBy (start <|> end) *> Parsec.anyChar
    start = Parsec.try (Parsec.string "/*")
    end = Parsec.string "*/"

  skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
  a `skipManyTill` b = void (Parsec.try b) <|> a *> (a `skipManyTill` b)

tokensTokenizer :: Tokenizer [Located Token]
tokensTokenizer = tokenTokenizer `Parsec.sepEndBy` silenceTokenizer

rangedTokenizer :: Tokenizer Token -> Tokenizer (Located Token)
rangedTokenizer parser = do
  column <- Parsec.getState
  begin <- Parsec.getPosition
  result <- parser
  end <- Parsec.getPosition
  return $ At (Origin.range begin end) (Indent (Just column)) result

tokenTokenizer :: Tokenizer (Located Token)
tokenTokenizer = rangedTokenizer $ Parsec.choice
  [ BlockBegin Layoutness.Nonlayout <$ Parsec.char '{'
  , BlockEnd <$ Parsec.char '}'
  , do
    let singleQuote = Parsec.char '\''
    mc <- Parsec.between singleQuote singleQuote $ character '\''
    case mc of
      Just c -> return (Character c)
      Nothing -> Parsec.unexpected "empty character literal"
  , Comma <$ Parsec.char ','
  , Parsec.try $ Ellipsis <$ Parsec.string "..."
  , GroupBegin <$ Parsec.char '('
  , GroupEnd <$ Parsec.char ')'
  , Parsec.try $ Ignore <$ Parsec.char '_' <* Parsec.notFollowedBy letter
  , Parsec.try $ VocabLookup <$ Parsec.string "::"
  , Layout <$ Parsec.char ':'
  , VectorBegin <$ Parsec.char '['
  , VectorEnd <$ Parsec.char ']'
  , Reference <$ Parsec.char '\\'
  , Text <$> Parsec.between (Parsec.char '"') (Parsec.char '"') text
  , Parsec.try $ do
    sign <- Parsec.optionMaybe (Parsec.oneOf "+-")
    let
      applySign :: (Num a) => Maybe Char -> a -> a
      applySign s = if s == Just '-' then negate else id
      base
        :: (Num a)
        => Char -> String -> (String -> a) -> Base -> String
        -> Tokenizer (Base, a)
      base prefix digits readBase hint desc = (,) hint . readBase
        <$> (Parsec.char prefix
          *> Parsec.many1 (Parsec.oneOf digits <?> (desc ++ " digit")))
    Parsec.choice
      [ Parsec.try
        $ fmap (\ (hint, value) -> Integer (applySign sign value) hint)
        $ Parsec.char '0' *> Parsec.choice
        [ base 'b' "01" readBin Binary "binary"
        , base 'o' ['0'..'7'] (fst . head . readOct) Octal "octal"
        , base 'x' (['0'..'9'] ++ ['A'..'F'])
          (fst . head . readHex) Hexadecimal "hexadecimal"
        ]
      , do
        integer <- Parsec.many1 Parsec.digit
        mFraction <- Parsec.optionMaybe
          $ Parsec.char '.' *> Parsec.many Parsec.digit
        mPower <- Parsec.optionMaybe $ Parsec.oneOf "Ee" *> ((,)
          <$> Parsec.optionMaybe (Parsec.oneOf "+-")
          <*> Parsec.many1 Parsec.digit)
        return $ case (mFraction, mPower) of
          (Nothing, Nothing) -> Integer (applySign sign (read integer)) Decimal
          _ -> Float
            (applySign sign (read (integer ++ fromMaybe "" mFraction)))
            (fromMaybe 0 (fmap length mFraction))
            (fromMaybe 0 (fmap (\ (s, p) -> applySign s $ read p) mPower))
      ] <* Parsec.notFollowedBy Parsec.digit
  , Parsec.try (Arrow <$ Parsec.string "->" <* Parsec.notFollowedBy symbol)
  , let
    alphanumeric = (Text.pack .) . (:)
      <$> (letter <|> Parsec.char '_')
      <*> (many . Parsec.choice) [letter, Parsec.char '_', Parsec.digit]
    in Parsec.choice
      [ do
        name <- alphanumeric
        return $ case name of
          "about" -> About
          "call" -> Call
          "case" -> Case
          "define" -> Define
          "do" -> Do
          "elif" -> Elif
          "else" -> Else
          "if" -> If
          "infix" -> Infix
          "instance" -> Instance
          "intrinsic" -> Intrinsic
          "jump" -> Jump
          "match" -> Match
          "permission" -> Permission
          "return" -> Return
          "synonym" -> Synonym
          "trait" -> Trait
          "type" -> Type
          "vocab" -> Vocab
          "with" -> With
          _ -> Word (Unqualified name)

-- See note [Angle Brackets].

      , bracketOperator '<'
      , AngleBegin <$ Parsec.char '<'
      , bracketOperator '>'
      , AngleEnd <$ Parsec.char '>'
      , Operator . Unqualified . Text.pack <$> Parsec.many1 symbol
      ]
  ]
  where

  bracketOperator :: Char -> Tokenizer Token
  bracketOperator char = Operator (Unqualified (Text.singleton char))
    <$ Parsec.try (Parsec.char char <* Parsec.notFollowedBy symbol)

  character :: Char -> Tokenizer (Maybe Char)
  character quote = Just <$> Parsec.noneOf ('\\' : [quote]) <|> escape

  escape :: Tokenizer (Maybe Char)
  escape = Parsec.char '\\' *> Parsec.choice
    [ Just <$> Parsec.oneOf "\\\"'"
    , Just '\a' <$ Parsec.char 'a'
    , Just '\b' <$ Parsec.char 'b'
    , Just '\f' <$ Parsec.char 'f'
    , Just '\n' <$ Parsec.char 'n'
    , Just '\r' <$ Parsec.char 'r'
    , Just '\t' <$ Parsec.char 't'
    , Just '\v' <$ Parsec.char 'v'
    , Just <$> (Parsec.space <* Parsec.spaces)
    , Nothing <$ Parsec.char '&'
    ]

  letter :: Tokenizer Char
  letter = Parsec.satisfy isLetter

  readBin :: String -> Integer
  readBin = go 0
    where
    go :: Integer -> [Char] -> Integer
    go acc ds = case ds of
      '0' : ds' -> go (2 * acc + 0) ds'
      '1' : ds' -> go (2 * acc + 1) ds'
      [] -> acc
      _ -> error "non-binary digit"

  special :: Tokenizer Char
  special = Parsec.oneOf "\"'(),:[\\]_{}"

  symbol :: Tokenizer Char
  symbol = Parsec.notFollowedBy special
    *> Parsec.choice (map Parsec.satisfy [isSymbol, isPunctuation])

  text :: Tokenizer Text
  text = Text.pack . catMaybes <$> many (character '"')
