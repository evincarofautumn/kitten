{-|
Module      : Kitten.Tokenize
Description : Lexing
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Tokenize
  ( tokenize
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isLetter, isPunctuation, isSymbol)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Traversable (forM)
import Kitten.Base (Base(..))
import Kitten.Bits
import Kitten.Indent (Indent(..))
import Kitten.Informer (Informer(..))
import Kitten.Layout (layout)
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
import qualified Text.Parsec.Pos as Parsec

-- | Lexes a source fragment into a list of tokens, annotated with their source
-- locations and indent levels.

tokenize
  :: (Informer m)
  => Int
  -- ^ Initial source line.
  -> FilePath
  -- ^ Source file path.
  -> Text
  -- ^ Source text.
  -> m [Located Token]
  -- ^ Lexed tokens.
tokenize line path text = case Parsec.runParser
  (setPos *> fileTokenizer) 1 path text of
  Left parseError -> do
    report $ Report.parseError parseError
    halt
  Right result -> layout path result
  where
  setPos = Parsec.setPosition $ Parsec.newPos path line 1

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
  return $ At (Origin.range begin end) (Indent column) result

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
  , do
    mc <- Parsec.between (Parsec.char '\x2018') (Parsec.char '\x2019')
      $ nestableCharacter '\x2018' '\x2019'
    case mc of
      [c] -> return (Character c)
      [] -> Parsec.unexpected "empty character literal"
      _ -> Parsec.unexpected "multi-character literal"
  , Comma <$ Parsec.char ','
  , Parsec.try $ Ellipsis
    <$ Parsec.choice (map Parsec.string ["...", "\x2026"])
  , GroupBegin <$ Parsec.char '('
  , GroupEnd <$ Parsec.char ')'
  , Parsec.try $ Ignore <$ Parsec.char '_' <* Parsec.notFollowedBy letter
  , Parsec.try $ VocabLookup
    <$ Parsec.choice (map Parsec.string ["::", "\x2237"])
  , Layout <$ Parsec.char ':'
  , VectorBegin <$ Parsec.char '['
  , VectorEnd <$ Parsec.char ']'
  , Reference <$ Parsec.char '\\'
  , Text <$> paragraph
  , Text <$> Parsec.between (Parsec.char '"') (Parsec.char '"') text
  , Text <$> Parsec.between (Parsec.char '\x201C') (Parsec.char '\x201D')
    (nestableText '\x201C' '\x201D')
  , Parsec.try $ do
    sign <- Parsec.optionMaybe (Parsec.oneOf "+-\x2212")
    let
      applySign :: (Num a) => Maybe Char -> a -> a
      applySign s = if s `elem` [Just '-', Just '\x2212'] then negate else id
      base
        :: (Num a)
        => Char -> String -> (String -> a) -> Base -> String
        -> Tokenizer (Base, a)
      base prefix digits readBase hint desc = (,) hint . readBase
        <$> (Parsec.char prefix
          *> Parsec.many1 (Parsec.oneOf digits <?> (desc ++ " digit")))
      integerBits = Parsec.option Signed32 $ Parsec.choice
        [ Parsec.char 'i' *> Parsec.choice
          [ Signed8 <$ Parsec.string "8"
          , Signed16 <$ Parsec.string "16"
          , Signed32 <$ Parsec.string "32"
          , Signed64 <$ Parsec.string "64"
          ]
        , Parsec.char 'u' *> Parsec.choice
          [ Unsigned8 <$ Parsec.string "8"
          , Unsigned16 <$ Parsec.string "16"
          , Unsigned32 <$ Parsec.string "32"
          , Unsigned64 <$ Parsec.string "64"
          ]
        ]
    Parsec.choice
      [ Parsec.try $ do
        (hint, value) <- Parsec.char '0' *> Parsec.choice
          [ base 'b' "01" readBin Binary "binary"
          , base 'o' ['0'..'7'] (fst . head . readOct) Octal "octal"
          , base 'x' (['0'..'9'] ++ ['A'..'F'])
            (fst . head . readHex) Hexadecimal "hexadecimal"
          ]
        bits <- integerBits
        return $ Integer (applySign sign value) hint bits
      , do
        integer <- Parsec.many1 Parsec.digit
        mFraction <- Parsec.optionMaybe
          $ Parsec.char '.' *> Parsec.many Parsec.digit
        mPower <- Parsec.optionMaybe $ Parsec.oneOf "Ee" *> ((,)
          <$> Parsec.optionMaybe (Parsec.oneOf "+-\x2212")
          <*> Parsec.many1 Parsec.digit)
        case (mFraction, mPower) of
          (Nothing, Nothing) -> do
            bits <- integerBits
            return $ Integer
              (applySign sign (read integer))
              Decimal
              bits
          _ -> do
            bits <- Parsec.option Float64 $ Parsec.char 'f' *> Parsec.choice
              [ Float32 <$ Parsec.string "32"
              , Float64 <$ Parsec.string "64"
              ]
            return $ Float
              (applySign sign (read (integer ++ fromMaybe "" mFraction)))
              (maybe 0 length mFraction)
              (maybe 0 (\ (s, p) -> applySign s $ read p) mPower)
              bits
      ] <* Parsec.notFollowedBy Parsec.digit
  , Parsec.try $ Arrow
    <$ (Parsec.choice $ map Parsec.string ["->", "\x2192"])
    <* Parsec.notFollowedBy symbol
  , let
    alphanumeric = (Text.pack .) . (:)
      <$> (letter <|> Parsec.char '_')
      <*> (many . Parsec.choice) [letter, Parsec.char '_', Parsec.digit]
    in Parsec.choice
      [ do
        name <- alphanumeric
        return $ case name of
          "about" -> About
          "as" -> As
          "case" -> Case
          "define" -> Define
          "do" -> Do
          "elif" -> Elif
          "else" -> Else
          "if" -> If
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

  nestableCharacter :: Char -> Char -> Tokenizer [Char]
  nestableCharacter open close = go
    where
      go = Parsec.choice
        [ (\ o t c -> o : t ++ [c])
          <$> (Parsec.char open <?> "nested opening quote")
          <*> (concat <$> Parsec.many go)
          <*> (Parsec.char close <?> "matching closing quote")
        , (:[]) <$> Parsec.noneOf ['\\', open, close]
        , maybeToList <$> escape
        ]

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
    go :: Integer -> String -> Integer
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

  nestableText :: Char -> Char -> Tokenizer Text
  nestableText open close = Text.pack . concat
    <$> many (nestableCharacter open close)

  paragraph :: Tokenizer Text
  paragraph = (<?> "paragraph") $ do
    _ <- Parsec.try $ Parsec.string "\"\"\""
    _ <- Parsec.char '\n' <?> "newline before paragraph body"
    (prefix, body) <- untilLeft paragraphLine
    body' <- forM body $ \ line -> case Text.stripPrefix prefix line of
      Just line' -> return line'
      Nothing | Text.null line -> return ""
      _ -> Parsec.unexpected (Text.unpack line)
        -- HACK: Relies on formatting of messages to include "expected ...".
        <?> concat
        [ "all lines to be empty or begin with "
        , show $ Text.length prefix
        , " spaces"
        ]
    return $ Text.intercalate "\n" body'

  paragraphLine :: Tokenizer (Either Text Text)
  paragraphLine = Parsec.choice
    [ (<?> "end of paragraph") $ Parsec.try $ Left . Text.pack
      <$> Parsec.many (Parsec.char ' ')
      <* Parsec.string "\"\"\""
    , (<?> "paragraph line") $ Right . Text.pack . catMaybes <$> Parsec.many
      (Parsec.choice [Just <$> Parsec.noneOf "\\\n", escape])
      <* Parsec.char '\n'
    ]

untilLeft :: (Monad m) => m (Either a b) -> m (a, [b])
untilLeft p = go []
  where
  go acc = do
    mx <- p
    case mx of
      Left a -> return (a, reverse acc)
      Right b -> go (b : acc)
