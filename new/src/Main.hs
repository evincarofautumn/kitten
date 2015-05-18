{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Function
import Data.Functor.Identity (Identity)
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Numeric
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import Text.Parsec (Column, ParsecT, SourcePos, (<?>))
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expression
import qualified Text.PrettyPrint as Pretty

main :: IO ()
main = do
  hSetEncoding stdout utf8
  reports <- newIORef []
  paths <- getArgs
  result <- runK (compile paths) Env { envContext = [], envReports = reports }
  case result of
    Nothing -> do
      paragraphs <- readIORef reports
      forM_ paragraphs $ \ paragraph -> do
        forM_ (reverse paragraph) $ hPutStrLn stderr . showReport
        hPutStrLn stderr ""
      exitFailure
    Just fragment -> putStrLn $ Pretty.render $ pPrint fragment




--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

compile :: [FilePath] -> K Fragment
compile paths = do
  sources <- liftIO $ mapM readFileUtf8 paths
  tokenized <- zipWithM tokenize paths sources
  checkpoint
  laidout <- zipWithM layout paths tokenized
  checkpoint
  parsed <- mconcat <$> zipWithM parse paths laidout
  checkpoint
  resolved <- resolveNames parsed
  checkpoint
  postfix <- desugarInfix resolved
  checkpoint
  let scoped = scope postfix
  desugared <- desugarDataDefinitions scoped
  -- TODO:
  -- infer types
  inferred <- inferTypes desugared
  -- lift closures into top-level definitions
  --   (possibly introducing a new "Flat" term type)
  --   value representation: def a (...) { ... $(local.0, local.1){ q... } ... }
  --                      -> def a (...) { ... local.0 local.1 \a::lambda0 new.act ... }
  --                         def b (...) { q... }
  -- make dup/drop explicit
  --   when copying from a local, insert dup
  --   when ignoring a value with ->_;, insert drop immediately
  --   when simply ignoring a local, insert drop at end of scope
  --     (warn about this unless prefixed with _, e.g., ->_ptr;)
  -- generate instantiations
  -- generate instance declarations
  --   (so inference can know what's available in a precompiled module)
  -- generate code
  return desugared
  where

  readFileUtf8 :: FilePath -> IO Text
  readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile




--------------------------------------------------------------------------------
-- Tokenization
--------------------------------------------------------------------------------

data Token
  = ArrowToken !Origin !Indent                   -- ->
  | BlockBeginToken !Layoutness !Origin !Indent  -- { :
  | BlockEndToken !Origin !Indent                -- }
  | BooleanToken !Bool !Origin !Indent           -- true false
  | CaseToken !Origin !Indent                    -- case
  | CharacterToken !Char !Origin !Indent         -- 'x'
  | CommaToken !Origin !Indent                   -- ,
  | DataToken !Origin !Indent                    -- data
  | DefineToken !Origin !Indent                  -- define
  | EllipsisToken !Origin !Indent                -- ...
  | ElifToken !Origin !Indent                    -- elif
  | ElseToken !Origin !Indent                    -- else
  | FloatToken !Double !Origin !Indent           -- 1.0
  | GroupBeginToken !Origin !Indent              -- (
  | GroupEndToken !Origin !Indent                -- )
  | IfToken !Origin !Indent                      -- if
  | IgnoreToken !Origin !Indent                  -- _
  | InfixToken !Origin !Indent                   -- infix
  | IntegerToken !Integer !Base !Origin !Indent  -- 1 0b1 0o1 0x1
  | LayoutToken !Origin !Indent                  -- :
  | MatchToken !Origin !Indent                   -- match
  | OperatorToken !Unqualified !Origin !Indent   -- +
  | ReferenceToken !Origin !Indent               -- \
  | SynonymToken !Origin !Indent                 -- synonym
  | TextToken !Text !Origin !Indent              -- "..."
  | VectorBeginToken !Origin !Indent             -- [
  | VectorEndToken !Origin !Indent               -- ]
  | VocabToken !Origin !Indent                   -- vocab
  | VocabLookupToken !Origin !Indent             -- ::
  | WordToken !Unqualified !Origin !Indent       -- word

-- A token origin.
data Origin
  = Range
    { rangeBegin :: !SourcePos
    , rangeEnd :: !SourcePos
    }
  | Anywhere
  deriving (Show)

type Indent = Maybe Column

data Layoutness = Layout | Nonlayout
  deriving (Show)

data Base = Binary | Octal | Decimal | Hexadecimal
  deriving (Show)

----------------------------------------

tokenize :: FilePath -> Text -> K [Token]
tokenize path text = case Parsec.runParser fileTokenizer 1 path text of
  Left parseError -> fail (show parseError)
  Right result -> return result

type Tokenizer a = ParsecT Text Column Identity a

fileTokenizer :: Tokenizer [Token]
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

tokensTokenizer :: Tokenizer [Token]
tokensTokenizer = tokenTokenizer `Parsec.sepEndBy` silenceTokenizer

rangedTokenizer :: Tokenizer (Origin -> Indent -> Token) -> Tokenizer Token
rangedTokenizer parser = do
  column <- Parsec.getState
  begin <- Parsec.getPosition
  result <- parser
  end <- Parsec.getPosition
  return $ result (Range begin end) (Just column)

tokenTokenizer :: Tokenizer Token
tokenTokenizer = rangedTokenizer $ Parsec.choice
  [ BlockBeginToken Nonlayout <$ Parsec.char '{'
  , BlockEndToken <$ Parsec.char '}'
  , do
    let singleQuote = Parsec.char '\''
    mc <- Parsec.between singleQuote singleQuote $ character '\''
    case mc of
      Just c -> return (CharacterToken c)
      Nothing -> Parsec.unexpected "empty character literal"
  , CommaToken <$ Parsec.char ','
  , Parsec.try $ EllipsisToken <$ Parsec.string "..."
  , GroupBeginToken <$ Parsec.char '('
  , GroupEndToken <$ Parsec.char ')'
  , Parsec.try $ IgnoreToken <$ Parsec.char '_' <* Parsec.notFollowedBy letter
  , Parsec.try $ VocabLookupToken <$ Parsec.string "::"
  , LayoutToken <$ Parsec.char ':'
  , VectorBeginToken <$ Parsec.char '['
  , VectorEndToken <$ Parsec.char ']'
  , ReferenceToken <$ Parsec.char '\\'
  , TextToken <$> Parsec.between (Parsec.char '"') (Parsec.char '"') text
  , Parsec.try $ do
    sign <- Parsec.optionMaybe (Parsec.oneOf "+-")
    let
      applySign :: (Num a) => a -> a
      applySign = if sign == Just '-' then negate else id
      base
        :: (Num a)
        => Char -> String -> (String -> a) -> Base -> String
        -> Tokenizer (Base, a)
      base prefix digits readBase hint desc = (,) hint . readBase
        <$> (Parsec.char prefix
          *> Parsec.many1 (Parsec.oneOf digits <?> (desc ++ " digit")))
    Parsec.choice
      [ Parsec.try
        $ fmap (\ (hint, value) -> IntegerToken (applySign value) hint)
        $ Parsec.char '0' *> Parsec.choice
        [ base 'b' "01" readBin Binary "binary"
        , base 'o' ['0'..'7'] (fst . head . readOct) Octal "octal"
        , base 'x' (['0'..'9'] ++ ['A'..'F'])
          (fst . head . readHex) Hexadecimal "hexadecimal"
        ]
      , do
        integer <- Parsec.many1 Parsec.digit
        mFraction <- Parsec.optionMaybe
          $ (:) <$> Parsec.char '.' <*> Parsec.many1 Parsec.digit
        return $ case mFraction of
          Just fraction -> FloatToken (applySign (read (integer ++ fraction)))
          Nothing -> IntegerToken (applySign (read integer)) Decimal
      ] <* Parsec.notFollowedBy Parsec.digit
  , Parsec.try (ArrowToken <$ Parsec.string "->" <* Parsec.notFollowedBy symbol)
  , let
    alphanumeric = (Text.pack .) . (:)
      <$> (letter <|> Parsec.char '_')
      <*> (many . Parsec.choice) [letter, Parsec.char '_', Parsec.digit]
    symbolic = Unqualified . Text.pack <$> Parsec.many1 symbol
    in Parsec.choice
      [ flip fmap alphanumeric $ \ name -> case name of
        "case" -> CaseToken
        "data" -> DataToken
        "def" -> DefineToken
        "elif" -> ElifToken
        "else" -> ElseToken
        "false" -> BooleanToken False
        "if" -> IfToken
        "infix" -> InfixToken
        "match" -> MatchToken
        "synonym" -> SynonymToken
        "true" -> BooleanToken True
        "vocab" -> VocabToken
        _ -> WordToken (Unqualified name)
      , OperatorToken <$> symbolic
      ]
  ]
  where

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

instance Eq Token where
  ArrowToken _ _        == ArrowToken _ _        = True
  BlockBeginToken _ _ _ == BlockBeginToken _ _ _ = True
  BlockEndToken _ _     == BlockEndToken _ _     = True
  BooleanToken a _ _    == BooleanToken b _ _    = a == b
  CaseToken _ _         == CaseToken _ _         = True
  CharacterToken a _ _  == CharacterToken b _ _  = a == b
  CommaToken _ _        == CommaToken _ _        = True
  DataToken _ _         == DataToken _ _         = True
  DefineToken _ _       == DefineToken _ _       = True
  EllipsisToken _ _     == EllipsisToken _ _     = True
  ElifToken _ _         == ElifToken _ _         = True
  ElseToken _ _         == ElseToken _ _         = True
  FloatToken a _ _      == FloatToken b _ _      = a == b
  GroupBeginToken _ _   == GroupBeginToken _ _   = True
  GroupEndToken _ _     == GroupEndToken _ _     = True
  IfToken _ _           == IfToken _ _           = True
  IgnoreToken _ _       == IgnoreToken _ _       = True
  InfixToken _ _        == InfixToken _ _        = True
  IntegerToken a _ _ _  == IntegerToken b _ _ _  = a == b
  LayoutToken _ _       == LayoutToken _ _       = True
  MatchToken _ _        == MatchToken _ _        = True
  OperatorToken a _ _   == OperatorToken b _ _   = a == b
  ReferenceToken _ _    == ReferenceToken _ _    = True
  SynonymToken _ _      == SynonymToken _ _      = True
  TextToken a _ _       == TextToken b _ _       = a == b
  VectorBeginToken _ _  == VectorBeginToken _ _  = True
  VectorEndToken _ _    == VectorEndToken _ _    = True
  VocabToken _ _        == VocabToken _ _        = True
  VocabLookupToken _ _  == VocabLookupToken _ _  = True
  WordToken a _ _       == WordToken b _ _       = a == b
  _                     == _                     = False

tokenOrigin :: Token -> Origin
tokenOrigin token = case token of
  ArrowToken origin _ -> origin
  BlockBeginToken _ origin _ -> origin
  BlockEndToken origin _ -> origin
  BooleanToken _ origin _ -> origin
  CaseToken origin _ -> origin
  CharacterToken _ origin _ -> origin
  CommaToken origin _ -> origin
  DataToken origin _ -> origin
  DefineToken origin _ -> origin
  EllipsisToken origin _ -> origin
  ElifToken origin _ -> origin
  ElseToken origin _ -> origin
  FloatToken _ origin _ -> origin
  GroupBeginToken origin _ -> origin
  GroupEndToken origin _ -> origin
  IfToken origin _ -> origin
  IgnoreToken origin _ -> origin
  InfixToken origin _ -> origin
  IntegerToken _ _ origin _ -> origin
  LayoutToken origin _ -> origin
  MatchToken origin _ -> origin
  OperatorToken _ origin _ -> origin
  ReferenceToken origin _ -> origin
  SynonymToken origin _ -> origin
  TextToken _ origin _ -> origin
  VectorBeginToken origin _ -> origin
  VectorEndToken origin _ -> origin
  VocabToken origin _ -> origin
  VocabLookupToken origin _ -> origin
  WordToken _ origin _ -> origin

tokenIndent :: Token -> Indent
tokenIndent token = case token of
  ArrowToken _ indent -> indent
  BlockBeginToken _ _ indent -> indent
  BlockEndToken _ indent -> indent
  BooleanToken _ _ indent -> indent
  CaseToken _ indent -> indent
  CharacterToken _ _ indent -> indent
  CommaToken _ indent -> indent
  DataToken _ indent -> indent
  DefineToken _ indent -> indent
  EllipsisToken _ indent -> indent
  ElifToken _ indent -> indent
  ElseToken _ indent -> indent
  FloatToken _ _ indent -> indent
  GroupBeginToken _ indent -> indent
  GroupEndToken _ indent -> indent
  IfToken _ indent -> indent
  IgnoreToken _ indent -> indent
  InfixToken _ indent -> indent
  IntegerToken _ _ _ indent -> indent
  LayoutToken _ indent -> indent
  MatchToken _ indent -> indent
  OperatorToken _ _ indent -> indent
  ReferenceToken _ indent -> indent
  SynonymToken _ indent -> indent
  TextToken _ _ indent -> indent
  VectorBeginToken _ indent -> indent
  VectorEndToken _ indent -> indent
  VocabToken _ indent -> indent
  VocabLookupToken _ indent -> indent
  WordToken _ _ indent -> indent

instance Show Token where
  show token = show (tokenOrigin token) ++ ": " ++ case token of
    ArrowToken{} -> "->"
    BlockBeginToken _ _ _ -> "{"
    BlockEndToken{} -> "}"
    BooleanToken True _ _ -> "true"
    BooleanToken False _ _ -> "false"
    CaseToken{} -> "case"
    CharacterToken c _ _ -> show c
    CommaToken{} -> ","
    DataToken{} -> "data"
    DefineToken{} -> "define"
    EllipsisToken{} -> "..."
    ElifToken{} -> "elif"
    ElseToken{} -> "else"
    FloatToken f _ _ -> show f
    GroupBeginToken{} -> "("
    GroupEndToken{} -> ")"
    IfToken{} -> "if"
    IgnoreToken{} -> "_"
    InfixToken{} -> "infix"
    IntegerToken value hint _ _ -> if value < 0 then '-' : shown else shown
      where
      shown = prefix ++ showIntAtBase base (digits !!) (abs value) ""
      (base, prefix, digits) = case hint of
        Binary -> (2, "0b", "01")
        Octal -> (8, "0o", ['0'..'7'])
        Decimal -> (10, "", ['0'..'9'])
        Hexadecimal -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
    LayoutToken{} -> ":"
    MatchToken{} -> "match"
    OperatorToken name _ _ -> show name
    ReferenceToken{} -> "\\"
    SynonymToken{} -> "synonym"
    TextToken t _ _ -> show t
    VectorBeginToken{} -> "["
    VectorEndToken{} -> "]"
    VocabToken{} -> "vocab"
    VocabLookupToken{} -> "::"
    WordToken name _ _ -> show name




--------------------------------------------------------------------------------
-- Layout and Parsing
--------------------------------------------------------------------------------

type Parser a = ParsecT [Token] Qualifier Identity a




----------------------------------------
-- Layout
----------------------------------------




layout :: FilePath -> [Token] -> K [Token]
layout path tokens
  = case Parsec.runParser insertBraces (Qualifier []) path tokens of
    Left parseError -> fail (show parseError)
    Right result -> return result

insertBraces :: Parser [Token]
insertBraces = (concat <$> many unit) <* Parsec.eof
  where

  unit :: Parser [Token]
  unit = unitWhere (const True)

  unitWhere :: (Token -> Bool) -> Parser [Token]
  unitWhere predicate
    = Parsec.try (Parsec.lookAhead (tokenSatisfy predicate)) *> Parsec.choice
      [ bracket (BlockBeginToken Nonlayout) BlockEndToken
      , bracket GroupBeginToken GroupEndToken
      , bracket VectorBeginToken VectorEndToken
      , layoutBlock
      , (:[]) <$> tokenSatisfy nonbracket
      ]

  bracket
    :: (Origin -> Indent -> Token) -> (Origin -> Indent -> Token)
    -> Parser [Token]
  bracket open close = do
    begin <- parserMatch open
    inner <- concat <$> many unit
    end <- parserMatch close
    return (begin : inner ++ [end])

  nonbracket :: Token -> Bool
  nonbracket = not . (`elem` brackets)

  brackets :: [Token]
  brackets = blockBrackets ++
    [ GroupBeginToken Anywhere Nothing
    , GroupEndToken Anywhere Nothing
    , VectorBeginToken Anywhere Nothing
    , VectorEndToken Anywhere Nothing
    ]

  blockBrackets :: [Token]
  blockBrackets =
    [ BlockBeginToken Nonlayout Anywhere Nothing
    , BlockEndToken Anywhere Nothing
    , LayoutToken Anywhere Nothing
    ]

  layoutBlock :: Parser [Token]
  layoutBlock = do
    colon <- parserMatch LayoutToken
    let
      colonOrigin = tokenOrigin colon
      colonIndent = tokenIndent colon
      validFirst token = let
        column = tokenIndent token
        in column > Just 1 && column >= colonIndent
    first <- Parsec.lookAhead (tokenSatisfy validFirst)
      <?> "token indented no less than start of layout block"
    let
      firstOrigin = rangeBegin (tokenOrigin first)
      inside token
        = Parsec.sourceColumn (rangeBegin (tokenOrigin token))
        >= Parsec.sourceColumn firstOrigin
    body <- concat <$> many (unitWhere inside)
    return $ BlockBeginToken Layout colonOrigin colonIndent
      : body ++ [BlockEndToken colonOrigin colonIndent]

tokenSatisfy :: (Token -> Bool) -> Parser Token
tokenSatisfy predicate = Parsec.tokenPrim show advance
  (\token -> if predicate token then Just token else Nothing)
  where
  advance :: SourcePos -> t -> [Token] -> SourcePos
  advance _ _ (token : _) = rangeBegin (tokenOrigin token)
  advance sourcePos _ _ = sourcePos

parserMatch :: (Origin -> Indent -> Token) -> Parser Token
parserMatch token = tokenSatisfy
  (== token Anywhere Nothing) <?> show (token Anywhere Nothing)

parserMatch_ :: (Origin -> Indent -> Token) -> Parser ()
parserMatch_ = void . parserMatch




----------------------------------------
-- Parsing
----------------------------------------




data Fragment = Fragment
  { fragmentDataDefinitions :: [DataDefinition]
  , fragmentDefinitions :: [Definition]
  , fragmentOperators :: [Operator]
  , fragmentSynonyms :: [Synonym]
  } deriving (Show)

instance Monoid Fragment where
  mempty = Fragment
    { fragmentDataDefinitions = []
    , fragmentDefinitions = []
    , fragmentOperators = []
    , fragmentSynonyms = []
    }
  mappend a b = Fragment
    { fragmentDataDefinitions
      = fragmentDataDefinitions a ++ fragmentDataDefinitions b
    , fragmentDefinitions
      = fragmentDefinitions a ++ fragmentDefinitions b
    , fragmentOperators
      = fragmentOperators a ++ fragmentOperators b
    , fragmentSynonyms
      = fragmentSynonyms a ++ fragmentSynonyms b
    }

data Element
  = DataDefinitionElement !DataDefinition
  | DefinitionElement !Definition
  | OperatorElement !Operator
  | SynonymElement !Synonym

data Definition = Definition
  { definitionBody :: !Term
  , definitionFixity :: !Fixity
  , definitionName :: !Qualified
  , definitionOrigin :: !Origin
  , definitionSignature :: !Signature
  } deriving (Show)

data DataDefinition = DataDefinition
  { dataConstructors :: [DataConstructor]
  , dataName :: !Qualified
  , dataOrigin :: !Origin
  , dataParameters :: [(Unqualified, Kind, Origin)]
  } deriving (Show)

data DataConstructor = DataConstructor
  { constructorFields :: [Signature]
  , constructorName :: !Unqualified
  , constructorOrigin :: !Origin
  } deriving (Show)

data Operator = Operator
  { operatorAssociativity :: !Associativity
  , operatorName :: !Qualified
  , operatorPrecedence :: !Precedence
  } deriving (Show)

data Fixity = Infix | Postfix
  deriving (Eq, Show)

data Synonym = Synonym !Qualified !GeneralName !Origin
  deriving (Show)

data Associativity = Nonassociative | Leftward | Rightward
  deriving (Show)

type Precedence = Int




-- This is the core language. It permits pushing values to the stack, invoking
-- definitions, and moving values between the stack and local variables.
--
-- It also permits empty programs and program concatenation. Together these form
-- a monoid over programs. The denotation of the concatenation of two programs
-- is the composition of the denotations of those two programs. In other words,
-- there is a homomorphism from the syntactic monoid onto the semantic monoid.

data Term
  = Call !(Maybe Type) !Fixity !GeneralName !Origin   -- f
  | Compose !(Maybe Type) !Term !Term                 -- e1 e2
  | Drop !(Maybe Type) !Origin                        -- drop
  | Generic !TypeId !Term !Origin                     -- Λx. e
  | Group !Term                                       -- (e)
  | Identity !(Maybe Type) !Origin                    --
  | If !(Maybe Type) !Term !Term !Origin              -- if { e1 } else { e2 }
  | Intrinsic !(Maybe Type) !Intrinsic !Origin        -- _::+
  | Lambda !(Maybe Type) !Unqualified !Term !Origin   -- → x; e
  | Match !(Maybe Type) [Case] !(Maybe Else) !Origin  -- match { case C {...}... else {...} }
  | New !(Maybe Type) !ConstructorIndex !Origin       -- new.n
  | Push !(Maybe Type) !Value !Origin                 -- push v
  | Swap !(Maybe Type) !Origin                        -- swap
  deriving (Show)

data Value
  = Boolean !Bool
  | Character !Char
  | Closed !ClosureIndex
  | Closure [Closed] !Term
  | Float !Double
  | Integer !Integer
  | Local !LocalIndex
  | Quotation !Term
  | Text !Text
  deriving (Show)

data Closed = ClosedLocal !LocalIndex | ClosedClosure !ClosureIndex
  deriving (Show)

type ClosureIndex = Int

data Case = Case !GeneralName !Term !Origin
  deriving (Show)

data Else = Else !Term !Origin
  deriving (Show)

type ConstructorIndex = Int

data Limit = Unlimited | Limit0 | Limit1
  deriving (Show)




----------------------------------------




parse :: FilePath -> [Token] -> K Fragment
parse name tokens = let
  parsed = Parsec.runParser fragmentParser globalVocabulary name tokens
  in case parsed of
    Left parseError -> fail (show parseError)
    Right result -> return result

fragmentParser :: Parser Fragment
fragmentParser = partitionElements <$> elementsParser <* Parsec.eof

elementsParser :: Parser [Element]
elementsParser = concat <$> many (vocabularyParser <|> (:[]) <$> elementParser)

partitionElements :: [Element] -> Fragment
partitionElements = foldr go mempty
  where
  go element acc = case element of
    DataDefinitionElement x -> acc
      { fragmentDataDefinitions = x : fragmentDataDefinitions acc }
    DefinitionElement x -> acc
      { fragmentDefinitions = x : fragmentDefinitions acc }
    OperatorElement x -> acc
      { fragmentOperators = x : fragmentOperators acc }
    SynonymElement x -> acc
      { fragmentSynonyms = x : fragmentSynonyms acc }

vocabularyParser :: Parser [Element]
vocabularyParser = do
  parserMatch_ VocabToken
  original@(Qualifier outer) <- Parsec.getState
  (vocabularyName, _) <- nameParser
  let
    (inner, name) = case vocabularyName of
      QualifiedName (Qualified (Qualifier qualifier) (Unqualified unqualified))
        -> (qualifier, unqualified)
      UnqualifiedName (Unqualified unqualified) -> ([], unqualified)
      LocalName{} -> error "local name should not appear as vocabulary name"
      IntrinsicName{} -> error
        "intrinsic name should not appear as vocabulary name"
  Parsec.putState (Qualifier (outer ++ inner ++ [name]))
  Parsec.choice
    [ [] <$ parserMatchOperator ";"
    , do
      elements <- blockedParser elementsParser
      Parsec.putState original
      return elements
    ]

blockedParser :: Parser a -> Parser a
blockedParser = Parsec.between
  (parserMatch (BlockBeginToken Nonlayout)) (parserMatch BlockEndToken)

groupedParser :: Parser a -> Parser a
groupedParser = Parsec.between
  (parserMatch GroupBeginToken) (parserMatch GroupEndToken)

groupParser :: Parser Term
groupParser = do
  origin <- getOrigin
  groupedParser $ Group . compose origin <$> Parsec.many1 termParser

angledParser :: Parser a -> Parser a
angledParser = Parsec.between (parserMatchOperator "<") (parserMatchOperator ">")

nameParser :: Parser (GeneralName, Fixity)
nameParser = do
  global <- isJust <$> Parsec.optionMaybe
    (parserMatch IgnoreToken <* parserMatch VocabLookupToken)
  parts <- ((,) Postfix <$> wordNameParser <|> (,) Infix <$> operatorNameParser)
    `Parsec.sepBy1` parserMatch VocabLookupToken
  return $ case parts of
    [(fixity, unqualified)]
      -> ((if global
        then QualifiedName . Qualified globalVocabulary
        else UnqualifiedName)
      unqualified, fixity)
    _ -> let
      parts' = map ((\ (Unqualified part) -> part) . snd) parts
      qualifier = init parts'
      (fixity, unqualified) = last parts
      in (QualifiedName (Qualified
        (Qualifier
          ((if global then (globalVocabularyName :) else id) qualifier))
        unqualified), fixity)

unqualifiedNameParser :: Parser Unqualified
unqualifiedNameParser = wordNameParser <|> operatorNameParser

wordNameParser :: Parser Unqualified
wordNameParser = parseOne $ \ token -> case token of
  WordToken name _ _ -> Just name
  _ -> Nothing

operatorNameParser :: Parser Unqualified
operatorNameParser = parseOne $ \ token -> case token of
  OperatorToken name _ _ -> Just name
  _ -> Nothing

parseOne :: (Token -> Maybe a) -> Parser a
parseOne = Parsec.tokenPrim show advance
  where
  advance :: SourcePos -> t -> [Token] -> SourcePos
  advance _ _ (token : _) = rangeBegin $ tokenOrigin token
  advance sourcePos _ _ = sourcePos

elementParser :: Parser Element
elementParser = Parsec.choice
  [ DataDefinitionElement <$> dataDefinitionParser
  , DefinitionElement <$> definitionParser
  , OperatorElement <$> operatorParser
  , SynonymElement <$> synonymParser
  ]

synonymParser :: Parser Synonym
synonymParser = (<?> "synonym") $ do
  origin <- getOrigin <* parserMatch_ SynonymToken
  from <- Qualified <$> Parsec.getState
    <*> (wordNameParser <|> operatorNameParser)
  (to, _) <- nameParser
  return $ Synonym from to origin

operatorParser :: Parser Operator
operatorParser = do
  parserMatch_ InfixToken
  (associativity, precedence) <- Parsec.choice
    [ (,) Nonassociative <$> precedenceParser
    , (,) Leftward
      <$> (parserMatch_ (WordToken (Unqualified "left")) *> precedenceParser)
    , (,) Rightward
      <$> (parserMatch_ (WordToken (Unqualified "right")) *> precedenceParser)
    ]
  name <- Qualified <$> Parsec.getState <*> operatorNameParser
  return Operator
    { operatorAssociativity = associativity
    , operatorName = name
    , operatorPrecedence = precedence
    }
  where
  precedenceParser :: Parser Precedence
  precedenceParser = (<?> "decimal integer precedence from 0 to 9")
    $ parseOne $ \ token -> case token of
      IntegerToken value Decimal _ _
        | value >= 0 && value <= 9 -> Just $ fromInteger value
      _ -> Nothing

dataDefinitionParser :: Parser DataDefinition
dataDefinitionParser = (<?> "data definition") $ do
  origin <- getOrigin <* parserMatch DataToken
  name <- Qualified <$> Parsec.getState
    <*> (wordNameParser <?> "data definition name")
  parameters <- Parsec.option [] quantifierParser
  constructors <- blockedParser $ many constructorParser
  return DataDefinition
    { dataConstructors = constructors
    , dataName = name
    , dataOrigin = origin
    , dataParameters = parameters
    }

constructorParser :: Parser DataConstructor
constructorParser = (<?> "data constructor") $ do
  origin <- getOrigin <* parserMatch CaseToken
  name <- wordNameParser <?> "constructor name"
  fields <- Parsec.option [] $ groupedParser
    $ typeParser `Parsec.sepEndBy` parserMatch CommaToken
  return DataConstructor
    { constructorFields = fields
    , constructorName = name
    , constructorOrigin = origin
    }

typeParser :: Parser Signature
typeParser = Parsec.try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  (stackEffect, origin) <- Parsec.choice
    [ Parsec.try $ do
      leftVar <- stack
      leftTypes <- Parsec.option [] (parserMatch CommaToken *> left)
      origin <- arrow
      rightVar <- stack
      rightTypes <- Parsec.option [] (parserMatch CommaToken *> right)
      return (StackFunctionSignature
        leftVar leftTypes rightVar rightTypes, origin)
    , do
      leftTypes <- left
      origin <- arrow
      rightTypes <- right
      return (FunctionSignature leftTypes rightTypes, origin)
    ]
  sideEffects <- many $ parserMatchOperator "+" *> (fst <$> nameParser)
  return (stackEffect sideEffects origin)
  where

  stack :: Parser Unqualified
  stack = wordNameParser <* parserMatch EllipsisToken

  left, right :: Parser [Signature]
  left = basicTypeParser `Parsec.sepEndBy` comma
  right = typeParser `Parsec.sepEndBy` comma

  comma :: Parser ()
  comma = void $ parserMatch CommaToken

  arrow :: Parser Origin
  arrow = getOrigin <* parserMatch ArrowToken

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  let makeSignature a b = ApplicationSignature a b $ signatureOrigin a
  fmap (foldl1' makeSignature) $ Parsec.many1 $ Parsec.choice
    [ quantifiedParser $ groupedParser typeParser
    , Parsec.try $ do
      origin <- getOrigin
      (name, fixity) <- nameParser
      -- Must be a word, not an operator, but may be qualified.
      guard $ fixity == Postfix
      return $ SignatureVariable name origin
    , groupedParser typeParser
    ]

quantifierParser :: Parser [(Unqualified, Kind, Origin)]
quantifierParser = angledParser $ var `Parsec.sepEndBy1` parserMatch CommaToken
  where

  var :: Parser (Unqualified, Kind, Origin)
  var = do
    origin <- getOrigin
    Parsec.choice
      [ (\ unqualified -> (unqualified, Effect, origin))
        <$> (parserMatchOperator "+" *> wordNameParser)
      , do
        name <- wordNameParser
        (\ effect -> (name, effect, origin))
          <$> Parsec.option Value (Stack <$ parserMatch EllipsisToken)
      ]

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getOrigin
  QuantifiedSignature <$> quantifierParser <*> thing <*> pure origin

definitionParser :: Parser Definition
definitionParser = (<?> "definition") $ do
  origin <- getOrigin <* parserMatch DefineToken
  (fixity, suffix) <- Parsec.choice
    [ (,) Postfix <$> wordNameParser
    , (,) Infix <$> operatorNameParser
    ] <?> "definition name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  signature <- signatureParser
  body <- blockParser <|> blockLambdaParser <?> "definition body"
  return Definition
    { definitionBody = body
    , definitionFixity = fixity
    , definitionName = name
    , definitionOrigin = origin
    , definitionSignature = signature
    }

signatureParser :: Parser Signature
signatureParser = quantifiedParser signature <|> signature <?> "type signature"
  where signature = groupedParser functionTypeParser

blockParser :: Parser Term
blockParser = blockedParser blockContentsParser <?> "block"

blockContentsParser :: Parser Term
blockContentsParser = do
  origin <- getOrigin
  terms <- many termParser
  let origin' = case terms of { x : _ -> termOrigin x; _ -> origin }
  return $ foldr (Compose Nothing) (Identity Nothing origin') terms

termParser :: Parser Term
termParser = do
  origin <- getOrigin
  Parsec.choice
    [ Parsec.try (uncurry (Push Nothing) <$> parseOne toLiteral <?> "literal")
    , do
      (name, fixity) <- nameParser
      return (Call Nothing fixity name origin)
    , Parsec.try sectionParser
    , Parsec.try groupParser <?> "parenthesized expression"
    , lambdaParser
    , matchParser
    , ifParser
    , Push Nothing <$> blockValue <*> pure origin
    ]
  where

  toLiteral :: Token -> Maybe (Value, Origin)
  toLiteral token = case token of
    BooleanToken x origin _ -> Just (Boolean x, origin)
    CharacterToken x origin _ -> Just (Character x, origin)
    FloatToken x origin _ -> Just (Float x, origin)
    IntegerToken x _ origin _ -> Just (Integer x, origin)
    TextToken x origin _ -> Just (Text x, origin)
    _ -> Nothing

  sectionParser :: Parser Term
  sectionParser = (<?> "operator section") $ groupedParser $ Parsec.choice
    [ do
      origin <- getOrigin
      function <- operatorNameParser
      let call = Call Nothing Postfix (UnqualifiedName function) origin
      Parsec.choice
        [ do
          operandOrigin <- getOrigin
          operand <- Parsec.many1 termParser
          return $ compose operandOrigin $ operand ++ [call]
        , return call
        ]
    , do
      operandOrigin <- getOrigin
      operand <- Parsec.many1
        $ Parsec.notFollowedBy operatorNameParser *> termParser
      origin <- getOrigin
      function <- operatorNameParser
      return $ compose operandOrigin $ operand ++
        [ Swap Nothing origin
        , Call Nothing Postfix (UnqualifiedName function) origin
        ]
    ]

  lambdaParser :: Parser Term
  lambdaParser = (<?> "variable introduction") $ Parsec.choice
    [ Parsec.try $ parserMatch ArrowToken *> do
      names <- lambdaNamesParser <* parserMatchOperator ";"
      origin <- getOrigin
      body <- blockContentsParser
      return $ makeLambda names body origin
    , do
      body <- blockLambdaParser
      let origin = termOrigin body
      return $ Push Nothing (Quotation body) origin
    ]

  matchParser :: Parser Term
  matchParser = (<?> "match") $ do
    matchOrigin <- getOrigin <* parserMatch MatchToken
    scrutineeOrigin <- getOrigin
    mScrutinee <- Parsec.optionMaybe groupParser <?> "scrutinee"
    (cases, mElse) <- blockedParser $ do
      cases' <- many $ (<?> "case") $ parserMatch CaseToken *> do
        origin <- getOrigin
        (name, _) <- nameParser
        body <- blockParser <|> blockLambdaParser
        return $ Case name body origin
      mElse' <- Parsec.optionMaybe $ do
        origin <- getOrigin <* parserMatch ElseToken
        body <- blockParser
        return $ Else body origin
      return (cases', mElse')
    let match = Match Nothing cases mElse matchOrigin
    return $ case mScrutinee of
      Just scrutinee -> compose scrutineeOrigin [scrutinee, match]
      Nothing -> match

  ifParser :: Parser Term
  ifParser = (<?> "if") $ do
    ifOrigin <- getOrigin <* parserMatch IfToken
    mCondition <- Parsec.optionMaybe groupParser <?> "condition"
    ifBody <- blockParser
    elifs <- many $ do
      origin <- getOrigin <* parserMatch ElifToken
      condition <- groupParser <?> "condition"
      body <- blockParser
      return (condition, body, origin)
    mElse <- Parsec.optionMaybe $ do
      origin <- getOrigin <* parserMatch ElseToken
      body <- blockParser
      return (Push Nothing (Boolean True) origin, body, origin)
    let
      desugarCondition (condition, body, origin) acc
        = compose ifOrigin [condition, If Nothing body acc origin]
    return $ foldr desugarCondition
      (Identity Nothing ifOrigin)
      ((fromMaybe (Identity Nothing ifOrigin) mCondition, ifBody, ifOrigin)
        : elifs ++ maybeToList mElse)

  blockValue :: Parser Value
  blockValue = (<?> "quotation") $ do
    origin <- getOrigin
    let
      reference = Call Nothing Postfix
        <$> (parserMatch ReferenceToken *> (fst <$> nameParser))
        <*> pure origin
    Quotation <$> (blockParser <|> reference)

parserMatchOperator :: Text -> Parser Token
parserMatchOperator = parserMatch . OperatorToken . Unqualified

lambdaBlockParser :: Parser ([(Maybe Unqualified, Origin)], Term, Origin)
lambdaBlockParser = parserMatch ArrowToken *> do
  names <- lambdaNamesParser
  origin <- getOrigin
  body <- blockParser
  return (names, body, origin)

lambdaNamesParser :: Parser [(Maybe Unqualified, Origin)]
lambdaNamesParser = Parsec.many1 $ do
  origin <- getOrigin
  name <- Just <$> wordNameParser <|> Nothing <$ parserMatch IgnoreToken
  return (name, origin)

blockLambdaParser :: Parser Term
blockLambdaParser = do
  (names, body, origin) <- lambdaBlockParser
  return (makeLambda names body origin)

makeLambda :: [(Maybe Unqualified, Origin)] -> Term -> Origin -> Term
makeLambda parsed body origin = foldr
  (\ (nameMaybe, nameOrigin) acc -> maybe
    (Compose Nothing (Drop Nothing origin) acc)
    (\ name -> Lambda Nothing name acc nameOrigin)
    nameMaybe)
  body
  (reverse parsed)

getOrigin :: (Monad m, Parsec.Stream s m c) => ParsecT s u m Origin
getOrigin = do
  start <- Parsec.getPosition
  return (Range start start)

compose :: Origin -> [Term] -> Term
compose origin = foldr (Compose Nothing) (Identity Nothing origin)

decompose :: Term -> [Term]
decompose (Compose _ a b) = decompose a ++ decompose b
decompose Identity{} = []
decompose term = [term]

termOrigin :: Term -> Origin
termOrigin term = case term of
  Call _ _ _ origin -> origin
  Compose _ a _ -> termOrigin a
  Drop _ origin -> origin
  Generic _ _ origin -> origin
  Group a -> termOrigin a
  Identity _ origin -> origin
  If _ _ _ origin -> origin
  Intrinsic _ _ origin -> origin
  Lambda _ _ _ origin -> origin
  New _ _ origin -> origin
  Match _ _ _ origin -> origin
  Push _ _ origin -> origin
  Swap _ origin -> origin

signatureOrigin :: Signature -> Origin
signatureOrigin signature = case signature of
  ApplicationSignature _ _ origin -> origin
  FunctionSignature _ _ _ origin -> origin
  QuantifiedSignature _ _ origin -> origin
  SignatureVariable _ origin -> origin
  StackFunctionSignature _ _ _ _ _ origin -> origin

--------------------------------------------------------------------------------
-- Name Resolution
--------------------------------------------------------------------------------




-- Names are complex. A qualified name consists of an unqualified name ('x')
-- plus a qualifier ('q::'). Name resolution is necessary because the referent
-- of an unqualified name is ambiguous without non-local knowledge.

data GeneralName
  = QualifiedName !Qualified
  | UnqualifiedName !Unqualified
  | LocalName !LocalIndex
  | IntrinsicName !Intrinsic
  deriving (Eq, Ord, Show)

data Qualified = Qualified
  { qualifierName :: !Qualifier
  , unqualifiedName :: !Unqualified
  } deriving (Eq, Ord, Show)

data Qualifier = Qualifier [Text]
  deriving (Eq, Ord, Show)

data Unqualified = Unqualified Text
  deriving (Eq, Ord, Show)

type LocalIndex = Int

data Intrinsic = AddIntrinsic
  deriving (Eq, Ord, Show)

type Resolved a = StateT [Unqualified] K a




-- Name resolution is responsible for rewriting unqualified calls to definitions
-- into fully qualified calls.

resolveNames :: Fragment -> K Fragment
resolveNames fragment = do
  reportDuplicateDefinitions
    $ map (definitionName &&& definitionOrigin) $ fragmentDefinitions fragment
  flip evalStateT [] $ do
    definitions <- mapM resolveDefinition $ fragmentDefinitions fragment
    return fragment { fragmentDefinitions = definitions }
  where

  resolveDefinition :: Definition -> Resolved Definition
  resolveDefinition definition = do
    let vocabulary = qualifierName $ definitionName definition
    body <- resolveTerm vocabulary $ definitionBody definition
    signature <- resolveSignature vocabulary $ definitionSignature definition
    return definition
      { definitionBody = body
      , definitionSignature = signature
      }

  resolveTerm :: Qualifier -> Term -> Resolved Term
  resolveTerm vocabulary = recur
    where

    recur :: Term -> Resolved Term
    recur unresolved = case unresolved of
      Call _ fixity name origin -> Call Nothing fixity
        <$> resolveDefinitionName vocabulary name origin <*> pure origin
      Compose _ a b -> Compose Nothing <$> recur a <*> recur b
      Drop{} -> return unresolved
      Generic{} -> error
        "generic expression should not appear before name resolution"
      Group a -> Group <$> recur a
      Identity{} -> return unresolved
      Intrinsic{} -> error
        "intrinsic name should not appear before name resolution"
      If _ a b origin -> If Nothing
        <$> recur a <*> recur b <*> pure origin
      Lambda _ name term origin -> withLocal name
        $ Lambda Nothing name <$> recur term <*> pure origin
      Match _ cases mElse origin -> Match Nothing
        <$> mapM resolveCase cases <*> traverse resolveElse mElse
        <*> pure origin
        where

        resolveCase :: Case -> Resolved Case
        resolveCase (Case name term caseOrigin) = do
          resolved <- resolveDefinitionName vocabulary name caseOrigin
          Case resolved <$> recur term <*> pure caseOrigin

        resolveElse :: Else -> Resolved Else
        resolveElse (Else term elseOrigin)
          = Else <$> recur term <*> pure elseOrigin

      New{} -> return unresolved
      Push _ value origin -> Push Nothing
        <$> resolveValue vocabulary value <*> pure origin
      Swap{} -> return unresolved

  resolveValue :: Qualifier -> Value -> Resolved Value
  resolveValue vocabulary value = case value of
    Boolean{} -> return value
    Character{} -> return value
    Closed{} -> error "closed name should not appear before name resolution"
    Closure{} -> error "closure should not appear before name resolution"
    Float{} -> return value
    Integer{} -> return value
    Local{} -> error "local name should not appear before name resolution"
    Quotation term -> Quotation <$> resolveTerm vocabulary term
    Text{} -> return value

  resolveSignature :: Qualifier -> Signature -> Resolved Signature
  resolveSignature vocabulary = go
    where

    go :: Signature -> Resolved Signature
    go signature = case signature of
      ApplicationSignature a b origin -> ApplicationSignature
        <$> go a <*> go b <*> pure origin
      FunctionSignature as bs es origin -> FunctionSignature
        <$> mapM go as <*> mapM go bs
        <*> mapM (uncurry (resolveTypeName vocabulary)) (zip es (repeat origin))
        <*> pure origin
      QuantifiedSignature vars a origin -> QuantifiedSignature vars
        <$> foldr withLocal (go a) (map (\ (name, _, _) -> name) vars)
        <*> pure origin
      SignatureVariable name origin -> SignatureVariable
        <$> resolveTypeName vocabulary name origin <*> pure origin
      StackFunctionSignature r as s bs es origin -> StackFunctionSignature r
        <$> mapM go as <*> pure s <*> mapM go bs
        <*> mapM (uncurry (resolveTypeName vocabulary)) (zip es (repeat origin))
        <*> pure origin

  resolveDefinitionName, resolveTypeName
    :: Qualifier -> GeneralName -> Origin -> Resolved GeneralName

  resolveDefinitionName = resolveName "word" $ flip Set.member defined
    where
    defined = Set.fromList $ map definitionName $ fragmentDefinitions fragment

  resolveTypeName = resolveName "type" $ flip Set.member defined
    where
    defined = Set.fromList $ map dataName $ fragmentDataDefinitions fragment

  resolveName
    :: Pretty.Doc -> (Qualified -> Bool) -> Qualifier -> GeneralName -> Origin
    -> Resolved GeneralName
  resolveName thing isDefined vocabulary name origin = case name of

-- An unqualified name may refer to a local, a name in the current vocabulary,
-- or a name in the global scope, respectively.

    UnqualifiedName unqualified -> do
      mLocalIndex <- gets (elemIndex unqualified)
      case mLocalIndex of
        Just index -> return (LocalName index)
        Nothing -> do
          let qualified = Qualified vocabulary unqualified
          if isDefined qualified then return (QualifiedName qualified) else do
            let global = Qualified globalVocabulary unqualified
            if isDefined global then return (QualifiedName global) else do
              lift $ report $ Report origin $ Pretty.hsep
                [ "undefined"
                , thing
                , Pretty.quotes (pPrint name)
                ]
              return name

-- A qualified name must be fully qualified, and may refer to an intrinsic or a
-- definition, respectively.

    QualifiedName qualified -> case intrinsicFromName qualified of
      Just intrinsic -> return (IntrinsicName intrinsic)
      Nothing -> do
        if isDefined qualified then return name else do
          lift $ report $ Report origin $ Pretty.hsep
            [ "undefined"
            , thing
            , Pretty.quotes (pPrint name)
            ]
          return name

    LocalName{} -> error "local name should not appear before name resolution"
    IntrinsicName{} -> error
      "intrinsic name should not appear before name resolution"

  withLocal :: Unqualified -> Resolved a -> Resolved a
  withLocal name action = do
    modify (name :)
    result <- action
    modify tail
    return result

reportDuplicateDefinitions :: [(Qualified, Origin)] -> K ()
reportDuplicateDefinitions _ = return ()

intrinsicFromName :: Qualified -> Maybe Intrinsic
intrinsicFromName name = case name of
  Qualified qualifier (Unqualified "+")
    | qualifier == globalVocabulary -> Just AddIntrinsic
  _ -> Nothing

qualifierFromName :: Qualified -> Qualifier
qualifierFromName (Qualified (Qualifier parts) (Unqualified name))
  = Qualifier (parts ++ [name])

globalVocabulary :: Qualifier
globalVocabulary = Qualifier [globalVocabularyName]

globalVocabularyName :: Text
globalVocabularyName = ""




--------------------------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------------------------




----------------------------------------
-- Infix Desugaring
----------------------------------------




type Rewriter a = ParsecT [Term] () Identity a

desugarInfix :: Fragment -> K Fragment
desugarInfix fragment = do
  desugared <- mapM desugarDefinition (fragmentDefinitions fragment)
  return fragment { fragmentDefinitions = desugared }
  where

  desugarDefinition :: Definition -> K Definition
  desugarDefinition definition = do
    desugared <- desugarTerms' (definitionBody definition)
    return definition { definitionBody = desugared }

  desugarTerms :: [Term] -> K Term
  desugarTerms terms = do
    terms' <- mapM desugarTerm terms
    let
      expression' = infixExpression <* Parsec.eof
      infixExpression = compose Anywhere  -- FIXME: Use better origin.
        <$> many (expression <|> lambda)
    case Parsec.runParser expression' () "" terms' of
      Left parseError -> do
        -- FIXME: Use better origin.
        -- FIXME: Pretty-print parse errors.
        report $ Report Anywhere $ Pretty.text $ show parseError
        return $ compose Anywhere terms
      Right result -> return result

  desugarTerm :: Term -> K Term
  desugarTerm term = case term of
    Call{} -> return term
    Compose _ a b -> desugarTerms (decompose a ++ decompose b)
    Drop{} -> return term
    Generic{} -> error
      "generic expression should not appear before infix desugaring"
    Group a -> desugarTerms' a
    Identity{} -> return term
    If _ a b origin -> If Nothing
      <$> desugarTerms' a <*> desugarTerms' b <*> pure origin
    Intrinsic{} -> return term
    Lambda _ name body origin -> Lambda Nothing name
      <$> desugarTerms' body <*> pure origin
    Match _ cases mElse origin -> Match Nothing
      <$> mapM desugarCase cases <*> traverse desugarElse mElse <*> pure origin
      where
      desugarCase :: Case -> K Case
      desugarCase (Case name body caseOrigin)
        = Case name <$> desugarTerms' body <*> pure caseOrigin
      desugarElse :: Else -> K Else
      desugarElse (Else body elseOrigin)
        = Else <$> desugarTerms' body <*> pure elseOrigin
    New{} -> return term
    Push _ value origin -> Push Nothing <$> desugarValue value <*> pure origin
    Swap{} -> return term

  desugarValue :: Value -> K Value
  desugarValue value = case value of
    Boolean{} -> return value
    Character{} -> return value
    Closed{} -> error "closed name should not appear before infix desugaring"
    Closure names body -> Closure names <$> desugarTerms' body
    Float{} -> return value
    Integer{} -> return value
    Local{} -> error "local name should not appear before infix desugaring"
    Quotation body -> Quotation <$> desugarTerms' body
    Text{} -> return value

  desugarTerms' = desugarTerms . decompose

  expression :: Rewriter Term
  expression = Expression.buildExpressionParser operatorTable operand
    where
    operand = (<?> "operand") $ do
      origin <- getOrigin
      results <- Parsec.many1 $ termSatisfy $ \ term -> case term of
        Call _ Infix _ _ -> False
        Lambda{} -> False
        _ -> True
      return (compose origin results)

  lambda :: Rewriter Term
  lambda = termSatisfy $ \ term -> case term of
    Lambda{} -> True
    _ -> False

  operatorTable :: [[Expression.Operator [Term] () Identity Term]]
  operatorTable = map (map toOperator) rawOperatorTable

  fragmentFixities :: Map Qualified Fixity
  fragmentFixities = Map.fromList
    $ map (definitionName &&& definitionFixity) $ fragmentDefinitions fragment

  rawOperatorTable :: [[Operator]]
  rawOperatorTable = let
    useDefault name fixity = fixity == Infix
      && not (any ((name ==) . operatorName) $ fragmentOperators fragment)
    flat = fragmentOperators fragment ++ map (\ name -> Operator
      { operatorAssociativity = Leftward
      , operatorName = name
      , operatorPrecedence = 6
      })
      (Map.keys (Map.filterWithKey useDefault fragmentFixities))
    in map (\ p -> filter ((p ==) . operatorPrecedence) flat) [9, 8 .. 0]

  toOperator :: Operator -> Expression.Operator [Term] () Identity Term
  toOperator operator = Expression.Infix
    (binaryOperator (QualifiedName (operatorName operator)))
    $ case operatorAssociativity operator of
      Nonassociative -> Expression.AssocNone
      Leftward -> Expression.AssocRight
      Rightward -> Expression.AssocLeft

  binaryOperator :: GeneralName -> Rewriter (Term -> Term -> Term)
  binaryOperator name = mapTerm $ \ term -> case term of
    Call _ Infix name' origin | name == name' -> Just (binary name origin)
    _ -> Nothing

  binary :: GeneralName -> Origin -> Term -> Term -> Term
  binary name origin x y = compose origin
    [x, y, Call Nothing Postfix name origin]

  mapTerm :: (Term -> Maybe a) -> Rewriter a
  mapTerm = Parsec.tokenPrim show advanceTerm

  termSatisfy :: (Term -> Bool) -> Rewriter Term
  termSatisfy predicate = Parsec.tokenPrim show advanceTerm
    (\ token -> if predicate token then Just token else Nothing)

  advanceTerm :: SourcePos -> t -> [Term] -> SourcePos
  advanceTerm _ _ (term : _) = rangeBegin (termOrigin term)
  advanceTerm sourcePos _ _ = sourcePos




----------------------------------------
-- Data Definition Desugaring
----------------------------------------




desugarDataDefinitions :: Fragment -> K Fragment
desugarDataDefinitions fragment = do
  definitions <- fmap concat $ mapM desugarDefinition
    $ fragmentDataDefinitions fragment
  return fragment { fragmentDefinitions
    = fragmentDefinitions fragment ++ definitions }
  where

  desugarDefinition :: DataDefinition -> K [Definition]
  desugarDefinition definition = mapM (uncurry desugarConstructor)
    $ zip [0..] $ dataConstructors definition
    where
    desugarConstructor :: Int -> DataConstructor -> K Definition
    desugarConstructor index constructor = do
      let
        resultSignature = foldl'
          (\ a b -> ApplicationSignature a b origin)
          (SignatureVariable (QualifiedName $ dataName definition)
            $ dataOrigin definition)
          $ map (\ (parameter, _, parameterOrigin)
            -> SignatureVariable (UnqualifiedName parameter) parameterOrigin)
          $ dataParameters definition
        constructorSignature = QuantifiedSignature (dataParameters definition)
          (FunctionSignature
            (constructorFields constructor) [resultSignature] [] origin)
          origin
      return Definition
        { definitionBody = New Nothing index $ constructorOrigin constructor
        , definitionFixity = Postfix
        , definitionName = Qualified qualifier $ constructorName constructor
        , definitionOrigin = origin
        , definitionSignature = constructorSignature
        }
      where
      origin = constructorOrigin constructor
      qualifier = qualifierFromName $ dataName definition




--------------------------------------------------------------------------------
-- Scope Resolution
--------------------------------------------------------------------------------




-- Whereas name resolution is concerned with resolving references to
-- definitions, scope resolution resolves references to locals and converts
-- quotations to use explicit closures.

scope :: Fragment -> Fragment
scope fragment = fragment
  { fragmentDefinitions = map scopeDefinition (fragmentDefinitions fragment) }
  where

  scopeDefinition :: Definition -> Definition
  scopeDefinition definition = definition
    { definitionBody = scopeTerm [0] (definitionBody definition) }

  scopeTerm :: [Int] -> Term -> Term
  scopeTerm stack = recur
    where
    recur term = case term of
      Call _ _ (LocalName index) origin
        -> Push Nothing (scopeValue stack (Local index)) origin
      Call{} -> term
      Compose _ a b -> Compose Nothing (recur a) (recur b)
      Drop{} -> term
      Generic{} -> error
        "generic expression should not appear before scope resolution"
      Group{} -> error
        "group expression should not appear after infix desugaring"
      Identity{} -> term
      If _ a b origin -> If Nothing (recur a) (recur b) origin
      Intrinsic{} -> term
      Lambda _ name a origin -> Lambda Nothing name
        (scopeTerm (mapHead succ stack) a) origin
      Match _ cases mElse origin -> Match Nothing
        (map (\ (Case name a caseOrigin)
          -> Case name (recur a) caseOrigin) cases)
        (fmap (\ (Else a elseOrigin)
          -> Else (recur a) elseOrigin) mElse)
        origin
      New{} -> term
      Push _ value origin -> Push Nothing (scopeValue stack value) origin
      Swap{} -> term

  scopeValue :: [Int] -> Value -> Value
  scopeValue stack value = case value of
    Boolean{} -> value
    Character{} -> value
    Closed{} -> error "closed name should not appear before scope resolution"
    Closure{} -> error "closure should not appear before scope resolution"
    Float{} -> value
    Integer{} -> value
    Local{} -> value
    Quotation body -> Closure (map ClosedLocal capturedNames) capturedTerm
      where

      capturedTerm :: Term
      capturedNames :: [LocalIndex]
      (capturedTerm, capturedNames) = runCapture stack' $ captureTerm scoped

      scoped :: Term
      scoped = scopeTerm stack' body

      stack' :: [Int]
      stack' = 0 : stack

    Text{} -> value

data ScopeEnv = ScopeEnv
  { scopeStack :: [ScopeDepth]
  , scopeDepth :: !ScopeDepth
  }

type ScopeDepth = Int

type Captured a = ReaderT ScopeEnv (State [LocalIndex]) a

runCapture :: [Int] -> Captured a -> (a, [LocalIndex])
runCapture stack = flip runState []
  . flip runReaderT ScopeEnv { scopeStack = stack, scopeDepth = 0 }

captureTerm :: Term -> Captured Term
captureTerm term = case term of
  Call{} -> return term
  Compose _ a b -> Compose Nothing <$> captureTerm a <*> captureTerm b
  Drop{} -> return term
  Generic{} -> error
    "generic expression should not appear before scope resolution"
  Group{} -> error
    "group expression should not appear after infix desugaring"
  Identity{} -> return term
  If _ a b origin -> If Nothing
    <$> captureTerm a <*> captureTerm b <*> pure origin
  Intrinsic{} -> return term
  Lambda _ name a origin -> let
    inside env = env
      { scopeStack = mapHead succ (scopeStack env)
      , scopeDepth = succ (scopeDepth env)
      }
    in Lambda Nothing name <$> local inside (captureTerm a) <*> pure origin
  Match _ cases mElse origin -> Match Nothing
    <$> mapM captureCase cases <*> traverse captureElse mElse <*> pure origin
    where

    captureCase :: Case -> Captured Case
    captureCase (Case name a caseOrigin)
      = Case name <$> captureTerm a <*> pure caseOrigin

    captureElse :: Else -> Captured Else
    captureElse (Else a elseOrigin)
      = Else <$> captureTerm a <*> pure elseOrigin

  New{} -> return term
  Push _ value origin -> Push Nothing <$> captureValue value <*> pure origin
  Swap{} -> return term

captureValue :: Value -> Captured Value
captureValue value = case value of
  Boolean{} -> return value
  Character{} -> return value
  Closed{} -> return value
  Closure names term -> Closure <$> mapM close names <*> pure term
    where

    close :: Closed -> Captured Closed
    close original = case original of
      ClosedLocal index -> do
        closed <- closeLocal index
        return $ case closed of
          Nothing -> original
          Just index' -> ClosedClosure index'
      ClosedClosure{} -> return original

  Float{} -> return value
  Integer{} -> return value
  Local index -> do
    closed <- closeLocal index
    return $ case closed of
      Nothing -> value
      Just index' -> Closed index'
  Quotation term -> let
    inside env = env { scopeStack = 0 : scopeStack env }
    in Quotation <$> local inside (captureTerm term)
  Text{} -> return value

closeLocal :: LocalIndex -> Captured (Maybe ClosureIndex)
closeLocal index = do
  stack <- asks scopeStack
  depth <- asks scopeDepth
  case stack of
    here : _ | index >= here -> Just <$> addName (index - depth)
    _ -> return Nothing
  where

  addName :: LocalIndex -> Captured ClosureIndex
  addName name = do
    names <- lift get
    case elemIndex name names of
      Just existing -> return existing
      Nothing -> do
        lift $ put $ names ++ [name]
        return $ length names

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs




--------------------------------------------------------------------------------
-- Type Inference
--------------------------------------------------------------------------------




-- A program consists of a set of definitions.

newtype Program = Program { programDefinitions :: Map Qualified (Type, Term) }

emptyProgram :: Program
emptyProgram = Program Map.empty




-- This is the type language. It describes a system of conventional Hindley–
-- Milner types, with type constructors joined by type application, as well as
-- type variables and constants for constraint solving and instance checking,
-- respectively. It syntactically permits higher-ranked quantification, though
-- there are semantic restrictions on this, discussed in the presentation of the
-- inference algorithm. Type variables have explicit kinds.

data Type
  = !Type :@ !Type
  | TypeConstructor !Constructor
  | TypeVar !Var
  | TypeConstant !Var
  | Forall !Var !Type
 deriving (Eq, Show)

newtype Constructor = Constructor Qualified
  deriving (Eq, Show)

data Var = Var !TypeId !Kind
  deriving (Eq, Show)

instance IsString Constructor where
  fromString = Constructor
    . Qualified globalVocabulary . Unqualified . Text.pack

instance IsString Type where
  fromString = TypeConstructor . fromString

(.->) :: Type -> Type -> Type -> Type
(t1 .-> t2) e = "fun" :@ t1 :@ t2 :@ e
infixr 4 .->

infixl 1 :@

(.*) :: Type -> Type -> Type
t1 .* t2 = "prod" :@ t1 :@ t2
infixl 5 .*

(.|) :: Type -> Type -> Type
t1 .| t2 = "join" :@ t1 :@ t2
infixr 4 .|




-- Type variables are distinguished by globally unique identifiers. This makes
-- it easier to support capture-avoiding substitution on types.

newtype TypeId = TypeId Int
  deriving (Enum, Eq, Ord, Show)




-- A kind (κ) is the type of a type. Types with the "value" kind (*) are
-- inhabited by values; all other types are used only to enforce program
-- invariants. These include:
--
--  • The "stack" kind (ρ), used to enforce that the stack cannot contain
--    other stacks.
--
--  • The "effect label" kind (λ), used to identify a side effect.
--
--  • The "effect" kind (ε), denoting a set of side effects.
--
--  • The "function" kind (κ → κ), used to describe type constructors.

data Kind = Value | Stack | Label | Effect | !Kind :-> !Kind
  deriving (Eq, Show)




-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What is the type of this type variable?
--  • What is the kind of this type variable?
--  • What is the type of this local variable?
--  • What are the types of the current closure?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TypeEnv = TypeEnv
  { tenvTvs :: !(Map TypeId Type)
  , tenvTks :: !(Map TypeId Kind)
  , tenvVs :: !(Map Unqualified Type)
  , tenvClosure :: [Type]
  , tenvSigs :: !(Map Qualified Type)
  , tenvCurrentType :: !(IORef TypeId)
  }

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv
  { tenvTvs = Map.empty
  , tenvTks = Map.empty
  , tenvVs = Map.empty
  , tenvClosure = []
  , tenvSigs = Map.empty
  , tenvCurrentType = currentTypeId
  }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

freshTv :: TypeEnv -> Kind -> K Type
freshTv tenv k = TypeVar <$> (Var <$> freshTypeId tenv <*> pure k)

freshTypeId :: TypeEnv -> K TypeId
freshTypeId tenv = do
  x <- liftIO $ readIORef $ tenvCurrentType tenv
  liftIO $ writeIORef (tenvCurrentType tenv) $ succ x
  return x




inferTypes :: Fragment -> K Program
inferTypes fragment = do
  let tenv0 = emptyTypeEnv
  definitions <- forM (fragmentDefinitions fragment) $ \ definition -> do
    type_ <- typeFromSignature tenv0 $ definitionSignature definition
    return (definitionName definition, (type_, definitionBody definition))
  let
    declaredTypes = Map.fromList
      $ map (\ (name, (type_, _)) -> (name, type_)) definitions
    infer = inferType0 declaredTypes
    go name (scheme, term) = do
      (term', scheme') <- infer term
      instanceCheck scheme' scheme
      let term'' = quantifyTerm scheme' term'
      return (name, (scheme', term''))
  definitions' <- mapM (uncurry go) definitions
  return Program { programDefinitions = Map.fromList definitions' }




-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0 :: Map Qualified Type -> Term -> K (Term, Type)
inferType0 sigs term = {- while ["inferring the type of", show term] $ -} do
  rec
    (term', t, tenvFinal) <- inferType tenvFinal
      emptyTypeEnv { tenvSigs = sigs } term
  let regeneralized = regeneralize tenvFinal (zonkType tenvFinal t)
  return (zonkTerm tenvFinal term', regeneralized)




-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType :: TypeEnv -> TypeEnv -> Term -> K (Term, Type, TypeEnv)
inferType _tenvFinal _tenv _term = error "TODO: inferType"




----------------------------------------
-- Unification
----------------------------------------



--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------




-- There are two kinds of unification going on here: basic logical unification
-- for value types, and row unification for effect types.

unifyType :: TypeEnv -> Type -> Type -> K TypeEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> return tenv0
  (TypeVar x, t) -> unifyTv tenv0 x t
  (_, TypeVar{}) -> commute
  -- FIXME: Unify the kinds here?
  (a, Forall (Var x k) t) -> do
    (b, _, tenv1) <- instantiate tenv0 x k t
    unifyType tenv1 a b
  (Forall{}, _) -> commute

-- The occurs check here prevents us from unifying rows with a common tail and a
-- distinct prefix, which could fail to terminate because the algorithm
-- generates fresh type variables.
--
-- See: Row Unification

  ("join" :@ l :@ r, s) -> do
    ms <- rowIso tenv0 l s
    case ms of
      Just ("join" :@ _ :@ s', substitution, tenv1) -> case substitution of
        Just (x, t)
          | occurs tenv0 x (effectTail r)
          -> fail $ unwords ["cannot unify effects", show t1, "and", show t2]
          | otherwise -> let
            tenv2 = tenv1 { tenvTvs = Map.insert x t $ tenvTvs tenv1 }
            in unifyType tenv2 r s'
        Nothing -> unifyType tenv1 r s'

      -- HACK: Duplicates the remaining cases.
      _ -> case (t1, t2) of
        (a :@ b, c :@ d) -> do
          tenv1 <- unifyType tenv0 a c
          unifyType tenv1 b d
        _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

  (_, "join" :@ _ :@ _) -> commute

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and effect type constructors.

  (a :@ b, c :@ d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d

  _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  commute = unifyType tenv0 t2 t1
  effectTail ("join" :@ _ :@ a) = effectTail a
  effectTail t = t




-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.
--
-- See: Occurs Checks

unifyTv :: TypeEnv -> Var -> Type -> K TypeEnv
unifyTv tenv0 v@(Var x _) t = case t of
  TypeVar (Var y _) | x == y -> return tenv0
  TypeVar{} -> declare
  _ -> if occurs tenv0 x t
    then let
      t' = zonkType tenv0 t
      in fail . unwords $ case t' of
        "prod" :@ _ :@ _ -> ["found mismatched stack depths solving", show v, "~", show t']
        _ -> [show v, "~", show t', "cannot be solved because", show x, "occurs in", show t']
    else declare
  where
  declare = case Map.lookup x $ tenvTvs tenv0 of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> return tenv0 { tenvTvs = Map.insert x t $ tenvTvs tenv0 }




-- A convenience function for unifying a type with a function type.

unifyFun :: TypeEnv -> Type -> K (Type, Type, Type, TypeEnv)
unifyFun tenv0 t = case t of
  "fun" :@ a :@ b :@ e -> return (a, b, e, tenv0)
  _ -> do
    a <- freshTv tenv0 Stack
    b <- freshTv tenv0 Stack
    e <- freshTv tenv0 Effect
    tenv1 <- unifyType tenv0 t ((a .-> b) e)
    return (a, b, e, tenv1)




-- Row unification is essentially unification of sets. The row-isomorphism
-- operation (as described in [1]) takes an effect label and an effect row, and
-- asserts that the row can be rewritten to begin with that label under some
-- substitution. It returns the substitution and the tail of the rewritten
-- row. The substitution is always either empty (∅) or a singleton substitution
-- (x ↦ τ).

rowIso
  :: TypeEnv -> Type -> Type
  -> K (Maybe (Type, Maybe (TypeId, Type), TypeEnv))

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 lin rin@("join" :@ l :@ _)
  | l == lin = return $ Just (rin, Nothing, tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l ("join" :@ l' :@ r)
  | l /= l' = do
  ms <- rowIso tenv0 l r
  return $ case ms of
    Just (r', substitution, tenv1) -> Just (l .| l' .| r', substitution, tenv1)
    Nothing -> Nothing

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail.

rowIso tenv0 l (TypeVar (Var a _)) = do
  b <- freshTv tenv0 Effect
  let res = l .| b
  return $ Just (res, Just (a, res), tenv0)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ = return Nothing




----------------------------------------
-- Desugaring
----------------------------------------




data Signature
  = ApplicationSignature Signature Signature !Origin
  | FunctionSignature [Signature] [Signature] [GeneralName] !Origin
  | QuantifiedSignature [(Unqualified, Kind, Origin)] !Signature !Origin
  | SignatureVariable !GeneralName !Origin
  | StackFunctionSignature
    !Unqualified [Signature] !Unqualified [Signature] [GeneralName] !Origin
  deriving (Show)




-- Here we desugar a parsed signature into an actual type. We resolve whether
-- names refer to quantified type variables or data definitions, and make stack
-- polymorphism explicit.

typeFromSignature :: TypeEnv -> Signature -> K Type
typeFromSignature tenv signature0 = do
  (type_, env) <- flip runStateT SignatureEnv
    { sigEnvAnonymous = []
    , sigEnvVars = Map.empty
    } $ go signature0
  return
    $ foldr (\ a -> Forall (Var a Stack))
      (foldr (\ (var, _origin) -> Forall var) type_ $ Map.elems $ sigEnvVars env)
    $ sigEnvAnonymous env
  where

  go :: Signature -> StateT SignatureEnv K Type
  go signature = case signature of
    ApplicationSignature a b _ -> (:@) <$> go a <*> go b
    FunctionSignature as bs es _ -> do
      r <- lift $ freshTypeId tenv
      let var = Var r Stack
      let typeVar = TypeVar var
      Forall var <$> makeFunction typeVar as typeVar bs es
    QuantifiedSignature vars a _ -> error "TODO: convert quantified signature"
      -- [(Unqualified, Kind, Origin)]
    SignatureVariable name origin -> fromVar name origin
    StackFunctionSignature r as s bs es origin -> do
      r' <- fromVar (UnqualifiedName r) origin
      s' <- fromVar (UnqualifiedName s) origin
      makeFunction r' as s' bs es

  fromVar :: GeneralName -> Origin -> StateT SignatureEnv K Type
  fromVar (UnqualifiedName name) origin = do
    existing <- gets $ Map.lookup name . sigEnvVars
    case existing of
      Just (var, _) -> return $ TypeVar var
      Nothing -> lift $ do
        report $ Report origin $ "unknown or undeclared type variable"
          Pretty.<+> Pretty.quotes (pPrint name)
        halt
  fromVar _ _ = error "TODO: fromVar"

  makeFunction
    :: Type -> [Signature] -> Type -> [Signature] -> [GeneralName]
    -> StateT SignatureEnv K Type
  makeFunction r as s bs es = do
    as' <- mapM go as
    bs' <- mapM go bs
    e <- lift $ freshTv tenv Effect
    return $ (stack r as' .-> stack s bs') $ foldl' (.|) e $ map fromEffect es
    where

    stack :: Type -> [Type] -> Type
    stack = foldl' (.*)

    fromEffect :: GeneralName -> Type
    fromEffect (QualifiedName name) = TypeConstructor $ Constructor name
    fromEffect _ = error
      "effect name should be fully qualified after name resolution"

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: [TypeId]
  , sigEnvVars :: !(Map Unqualified (Var, Origin))
  }




----------------------------------------
-- Zonking
----------------------------------------



-- Zonking a type fully substitutes all type variables. That is, if you have:
--
--     t0 ~ t1
--     t1 ~ int
--
-- Then zonking "t0" gives you "int".

zonkType :: TypeEnv -> Type -> Type
zonkType tenv0 = recur
  where
  recur t = case t of
    TypeConstructor{} -> t
    TypeVar (Var x k) -> case Map.lookup x (tenvTvs tenv0) of
      Just (TypeVar (Var x' _)) | x == x' -> TypeVar (Var x k)
      Just t' -> recur t'
      Nothing -> t
    TypeConstant{} -> t
    Forall (Var x k) t' -> Forall (Var x k)
      $ zonkType tenv0 { tenvTvs = Map.delete x (tenvTvs tenv0) } t'
    a :@ b -> recur a :@ recur b




-- Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.

zonkTerm :: TypeEnv -> Term -> Term
zonkTerm _tenv0 = error "TODO: zonkTerm"
{-
  where
  recur term = case term of
    EPush tref val -> EPush (zonkMay tref) val
    ECall tref name params -> ECall (zonkMay tref) name (map (zonkType tenv0) params)
    ECat tref e1 e2 -> ECat (zonkMay tref) (recur e1) (recur e2)
    EId tref -> EId (zonkMay tref)
    EGo tref name -> EGo (zonkMay tref) name
    ECome how tref name -> ECome how (zonkMay tref) name
    EForall{} -> error "cannot zonk generic expression"
    EIf tref e1 e2 -> EIf (zonkMay tref) (recur e1) (recur e2)
  zonkMay = fmap (zonkType tenv0)
-}




----------------------------------------
-- Instantiation
----------------------------------------



--------------------------------------------------------------------------------
-- Instantiation and Regeneralization
--------------------------------------------------------------------------------




-- To instantiate a type scheme, we simply replace all quantified variables with
-- fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order. Because type identifiers are globally
-- unique, we know a fresh type variable will never be erroneously captured.

instantiate :: TypeEnv -> TypeId -> Kind -> Type -> K (Type, Type, TypeEnv)
instantiate tenv0 x k t = do
  ia <- freshTypeId tenv0
  let a = TypeVar (Var ia k)
  replaced <- replaceTv tenv0 x a t
  return (replaced, a, tenv0 { tenvTks = Map.insert ia k $ tenvTks tenv0 })




-- When generating an instantiation of a generic definition, we only want to
-- instantiate the rank-1 quantifiers; all other quantifiers are irrelevant.

instantiatePrenex :: TypeEnv -> Type -> K (Type, [Type], TypeEnv)
instantiatePrenex tenv0 q@(Forall (Var x k) t)
  = {- while ["instantiating", show q] $ -} do
    (t', a, tenv1) <- instantiate
      tenv0 { tenvTks = Map.insert x k $ tenvTks tenv0 } x k t
    (t'', as, tenv2) <- instantiatePrenex tenv1 t'
    return (t'', a : as, tenv2)
instantiatePrenex tenv0 t = return (t, [], tenv0)




----------------------------------------
-- Regeneralization
----------------------------------------




-- Because all functions are polymorphic with respect to the part of the stack
-- they don't touch, all words of order n can be regeneralized to words of rank
-- n with respect to the stack-kinded type variables.
--
-- This means that if a stack-kinded (ρ) type variable occurs only twice in a
-- type, in the bottommost position on both sides of a function arrow, then its
-- scope can be reduced to only that function arrow by introducing a
-- higher-ranked quantifier. This is a more conservative rule than used in [3].
-- For example, the type of "map":
--
--     map :: ∀ρσαβ. ρ × vector α × (σ × α → σ × β) → ρ × vector β
--
-- Can be regeneralized like so:
--
--     map :: ∀ραβ. ρ × vector α × (∀σ. σ × α → σ × β) → ρ × vector β
--
-- In order to correctly regeneralize a type, it needs to contain no
-- higher-ranked quantifiers.

regeneralize :: TypeEnv -> Type -> Type
regeneralize tenv t = let
  (t', vars) = runWriter $ go t
  in foldr (uncurry ((Forall .) . Var)) t'
    $ foldr (deleteBy ((==) `on` fst)) (Map.toList (freeTvks t')) vars
  where
  go :: Type -> Writer [(TypeId, Kind)] Type
  go t' = case t' of
    TypeConstructor "fun" :@ a :@ b :@ e
      | TypeVar (Var c k) <- bottommost a
      , TypeVar (Var d _) <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [(c, k)]
        a' <- go a
        b' <- go b
        e' <- go e
        return $ Forall (Var c k) ((a' .-> b') e')
    TypeConstructor "prod" :@ a :@ b -> do
      a' <- go a
      b' <- go b
      return $ "prod" :@ a' :@ b'
    Forall{} -> error "cannot regeneralize higher-ranked type"
    a :@ b -> (:@) <$> go a <*> go b
    _ -> return t'




bottommost :: Type -> Type
bottommost (TypeConstructor "prod" :@ a :@ _) = bottommost a
bottommost a = a




----------------------------------------
-- Instance Checking
----------------------------------------




-- Since skolem constants only unify with type variables and themselves,
-- unifying a skolemized scheme with a type tells you whether one is a generic
-- instance of the other. This is used to check the signatures of definitions.

instanceCheck :: Type -> Type -> K ()
instanceCheck inferredScheme declaredScheme = do
  let tenv0 = emptyTypeEnv
  (inferredType, _, tenv1) <- instantiatePrenex tenv0 inferredScheme
  (ids, declaredType) <- skolemize tenv1 declaredScheme
  check <- K $ \ env -> do
    result <- runK (subsumptionCheck tenv1 inferredType declaredType) env
    return $ Just result
  case check of
    Nothing -> failure
    _ -> return ()
  let escaped = freeTvs inferredScheme `Set.union` freeTvs declaredScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) failure
  return ()
  where
  failure = report $ Report Anywhere $ Pretty.hsep
    [ pPrint inferredScheme
    , "is not an instance of"
    , pPrint declaredScheme
    ]




-- Skolemization replaces quantified type variables with type constants.

skolemize :: TypeEnv -> Type -> K (Set TypeId, Type)
skolemize tenv0 t = case t of
  Forall (Var x k) t' -> do
    c <- freshTypeId tenv0
    let tenv1 = tenv0 { tenvTvs = Map.insert x (TypeConstant (Var c k)) (tenvTvs tenv0) }
    (c', t'') <- skolemize tenv1 (zonkType tenv1 t')
    return (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  "fun" :@ a :@ b :@ e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, (a .-> b') e)
  _ -> return (Set.empty, t)




-- Subsumption checking is largely the same as unification, except for the fact
-- that a function type is contravariant in its input type.

subsumptionCheck :: TypeEnv -> Type -> Type -> K TypeEnv
subsumptionCheck tenv0 (Forall (Var x k) t) t2 = do
  (t1, _, tenv1) <- instantiate tenv0 x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 ("fun" :@ a :@ b :@ e) = do
  (a', b', e', tenv1) <- unifyFun tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 ("fun" :@ a' :@ b' :@ e') t2 = do
  (a, b, e, tenv1) <- unifyFun tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 t1 t2 = unifyType tenv0 t1 t2

subsumptionCheckFun
  :: TypeEnv -> Type -> Type -> Type -> Type -> Type -> Type -> K TypeEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  subsumptionCheck tenv2 e e'




----------------------------------------
-- Instance Generation
----------------------------------------





--------------------------------------------------------------------------------
-- Instance Generation
--------------------------------------------------------------------------------




-- In order to support unboxed generics, for every call site of a generic
-- definition in a program, we produce a specialized instantiation of the
-- definition with the value-kinded type parameters set to the given type
-- arguments. This is transitive: if a generic definition calls another generic
-- definition with one of its own generic type parameters as a type argument,
-- then an instantiation must also be generated of the called definition.

{-

collectInstantiations :: TypeEnv -> Program -> K Program
collectInstantiations tenv (Program defs0) = do

-- We first enqueue all the instantiation sites reachable from the top level of
-- the program, and any non-generic definitions.

  (expr1, q0) <- go emptyQueue expr0
  (defs1, q1) <- foldrM
    (\ (name, (type_, expr)) (acc, q) -> do
      (expr', q') <- go q expr
      return ((name, (type_, expr')) : acc, q'))
    ([], q0)
    (Map.toList defs0)

-- Next, we process the queue. Doing so may enqueue new instantiation sites for
-- processing; however, this is guaranteed to halt because the number of actual
-- instantiations is finite.

  defs2 <- processQueue q1 $ Map.fromList defs1
  return (defs2, expr1)
  where

  go :: Queue (Name, [Type]) -> Expr -> K (Expr, Queue (Name, [Type]))
  go q0 expr = case expr of
    EId{} -> return (expr, q0)
    ECat tref a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      return (ECat tref a' b', q2)
    ECall tref name args -> return (ECall tref (mangleName name args) [], enqueue (name, args) q0)
    EPush{} -> return (expr, q0)
    EGo{} -> return (expr, q0)
    ECome{} -> return (expr, q0)
    -- If the definition is generic, we simply ignore it; we won't find any
    -- instantiations in it, because it's not instantiated, itself!
    EForall{} -> return (expr, q0)
    EIf tref a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      return (EIf tref a' b', q2)

  processQueue
    :: Queue (Name, [Type]) -> Program -> K Program
  processQueue q (Program defs) = case dequeue q of
    Nothing -> return $ Program defs
    Just ((name, args), q') -> let
      mangled = mangleName name args
      in case Map.lookup mangled defs of
        Just{} -> processQueue q' defs
        Nothing -> case Map.lookup name defs of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' defs
          Just (type_, expr) -> do
            expr' <- instantiateExpr tenv expr args
            (expr'', q'') <- go q' expr'
            processQueue q'' $ Map.insert mangled (type_, expr'') defs




-- Names are mangled according to the local C++ mangling convention. This is a
-- silly approximation of the IA-64 convention for testing purposes.

mangleName :: Name -> [Type] -> Name
mangleName name args = case args of
  [] -> prefix <> lengthEncode name
  _ -> prefix <> lengthEncode name <> "I" <> Text.concat (map mangleType args) <> "E"
  where
  prefix = "_Z"

mangleType :: Type -> Name
mangleType ("fun" :@ _ :@ _) = "PFvv"
mangleType ("ptr" :@ a) = Text.concat ["P", mangleType a]
mangleType (TCon (Con con)) = case con of
  "int" -> "i"
  _ -> lengthEncode con
mangleType ("prod" :@ a :@ b) = Text.concat $ map mangleType [a, b]
mangleType _ = "?"

lengthEncode :: Name -> Name
lengthEncode name = Text.pack (show (Text.length name)) <> name




-- A generic queue with amortized O(1) enqueue/dequeue.

data Queue a = Queue [a] [a]

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue i (x : o)) = Just (x, Queue i o)
dequeue (Queue i@(_ : _) []) = dequeue (Queue [] (reverse i))
dequeue (Queue [] []) = Nothing

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x : i) o

emptyQueue :: Queue a
emptyQueue = Queue [] []

queueFromList :: [a] -> Queue a
queueFromList = Queue [] . reverse




-- Instantiates a generic expression with the given type arguments.

instantiateExpr :: TypeEnv -> Term -> [Type] -> K Term
instantiateExpr tenv = foldlM go
  where
  go (Generic x expr) arg = replaceTvExpr tenv x arg expr
  go _ _ = error "instantiateExpr: wrong number of type parameters"

-}




-- Copies the top-level generic value-kinded type quantifiers from a polytype to
-- an expression, thereby making the expression generic, e.g.:
--
--     ∀α:ρ. ∀β:*. ∀γ:Ε. (α × β → α × β × β) Ε    dup
--
--     Λβ:*. dup

quantifyTerm :: Type -> Term -> Term
quantifyTerm (Forall (Var x Value) t) e = Generic x (quantifyTerm t e) Anywhere
quantifyTerm (Forall _ t) e = quantifyTerm t e
quantifyTerm _ e = e




----------------------------------------
-- Substitution
----------------------------------------




-- Capture-avoiding substitution of a type variable α with a type τ throughout a
-- type σ, [α ↦ τ]σ.

replaceTv :: TypeEnv -> TypeId -> Type -> Type -> K Type
replaceTv tenv0 x a = recur
  where
  recur t = case t of
    Forall (Var x' k) t'
      | x == x' -> return t
      | x' `Set.notMember` freeTvs t' -> Forall (Var x' k) <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- replaceTv tenv0 x' (TypeVar (Var z k)) t'
        Forall (Var z k) <$> recur t''
    TypeVar (Var x' _) | x == x' -> return a
    m :@ n -> (:@) <$> recur m <*> recur n
    _ -> return t




replaceTvExpr :: TypeEnv -> TypeId -> Type -> Term -> K Term
replaceTvExpr tenv x a = error "TODO: replaceTvExpr"  -- recur
  where
{-
  recur expr = case expr of
    EId tref -> EId <$> go' tref
    ECat tref e1 e2 -> ECat <$> go' tref <*> recur e1 <*> recur e2
    ECall tref name args -> ECall <$> go' tref <*> pure name <*> mapM go args
    EPush tref val -> EPush <$> go' tref <*> pure val
    EGo tref name -> EGo <$> go' tref <*> pure name
    ECome how tref name -> ECome how <$> go' tref <*> pure name
    EForall x' _
      | x == x' -> return expr
      | otherwise -> error "substituting in a generic expression should not require capture-avoidance"
    EIf tref e1 e2 -> EIf <$> go' tref <*> recur e1 <*> recur e2
  go' (Just t) = Just <$> go t
  go' Nothing = return Nothing
  go t = replaceTv tenv x a t
-}




----------------------------------------
-- Free Variables
----------------------------------------




-- The free variables of a type are those not bound by any quantifier.

freeTvs :: Type -> Set TypeId
freeTvs = Set.fromList . Map.keys . freeTvks

freeTvks :: Type -> Map TypeId Kind
freeTvks t = case t of
  TypeConstructor{} -> Map.empty
  TypeVar (Var x k) -> Map.singleton x k
  TypeConstant{} -> Map.empty
  Forall (Var x _) t' -> Map.delete x (freeTvks t')
  a :@ b -> Map.union (freeTvks a) (freeTvks b)




----------------------------------------
-- Occurs Checks
----------------------------------------




-- We need to be able to count occurrences of a type variable in a type, not
-- just check for its presence. This is for two reasons: to prevent infinite
-- types (the "occurs check"), and to determine whether a stack variable can be
-- generalized to a higher rank.

occurrences :: TypeEnv -> TypeId -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TypeConstructor{} -> 0
    TypeVar (Var y _) -> case Map.lookup y (tenvTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TypeConstant{} -> 0
    Forall (Var x' _) t' -> if x == x' then 0 else recur t'
    a :@ b -> recur a + recur b

occurs :: TypeEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0




--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------




instance Pretty Fragment where
  pPrint fragment = Pretty.vcat (intersperse "" (concat docs))
    where

    docs :: [[Pretty.Doc]]
    docs =
      [ pretties (fragmentDataDefinitions fragment)
      , pretties (fragmentDefinitions fragment)
      , pretties (fragmentOperators fragment)
      , pretties (fragmentSynonyms fragment)
      ]

    pretties :: Pretty a => [a] -> [Pretty.Doc]
    pretties = map pPrint

-- FIXME: Support parameters.
instance Pretty DataDefinition where
  pPrint definition = Pretty.vcat
    [ "data"
      Pretty.<+> pPrint (dataName definition)
      Pretty.<> ":"
    , Pretty.nest 4 $ Pretty.vcat $ map pPrint $ dataConstructors definition
    ]

-- FIXME: Support fields.
instance Pretty DataConstructor where
  pPrint constructor = "case"
    Pretty.<+> pPrint (constructorName constructor)

instance Pretty Definition where
  pPrint definition = Pretty.vcat
    [ "def"
      Pretty.<+> pPrint (definitionName definition)
      Pretty.<+> pPrint (definitionSignature definition)
      Pretty.<> ":"
    , Pretty.nest 4 $ pPrint $ definitionBody definition
    ]

instance Pretty Operator where
  pPrint operator = Pretty.hsep
    $ ("infix" :)
    $ (case operatorAssociativity operator of
      Nonassociative -> id
      Leftward -> ("left" :)
      Rightward -> ("right" :))
    [ pPrint $ operatorPrecedence operator
    , pPrint $ operatorName operator
    ]

instance Pretty Term where
  pPrint term = case term of
    Call _ _ name _ -> pPrint name
    Compose _ a b -> pPrint a Pretty.$+$ pPrint b
    Drop _ _ -> "drop"
    Generic{} -> error "TODO: pretty-print generic expressions"
    Group a -> Pretty.parens (pPrint a)
    Identity{} -> Pretty.empty
    If _ a b _ -> "if:"
      Pretty.$$ Pretty.nest 4 (pPrint a)
      Pretty.$$ "else:"
      Pretty.$$ Pretty.nest 4 (pPrint b)
    Intrinsic _ name _ -> pPrint name
    Lambda _ name body _ -> "->"
      Pretty.<+> pPrint name
      Pretty.<> ";"
      Pretty.$+$ pPrint body
    Match{} -> error "TODO: pretty-print match expressions"
    New _ index _ -> "new." Pretty.<> pPrint index
    Push _ value _ -> pPrint value
    Swap{} -> "swap"

instance Pretty Value where
  pPrint value = case value of
    Boolean True -> "true"
    Boolean False -> "false"
    Character c -> Pretty.quotes $ Pretty.char c
    Closed index -> "closure." Pretty.<> Pretty.int index
    Closure names term -> Pretty.hcat
      [ Pretty.char '$'
      , Pretty.parens $ prettyList $ map pPrint names
      , Pretty.braces $ pPrint term
      ]
    Float f -> Pretty.double f
    Integer i -> Pretty.integer i
    Local index -> "local." Pretty.<> Pretty.int index
    Quotation body -> Pretty.braces $ pPrint body
    Text t -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t

instance Pretty Closed where
  pPrint (ClosedLocal index) = Pretty.hcat
    ["local.", Pretty.int index]
  pPrint (ClosedClosure index) = Pretty.hcat
    ["closure.", Pretty.int index]

instance Pretty Qualified where
  pPrint qualified = pPrint (qualifierName qualified)
    Pretty.<> "::" Pretty.<> pPrint (unqualifiedName qualified)

instance Pretty Qualifier where
  pPrint (Qualifier ("" : parts)) = pPrint $ Qualifier $ "_" : parts
  pPrint (Qualifier parts) = Pretty.text
    $ Text.unpack $ Text.intercalate "::" parts

instance Pretty Unqualified where
  pPrint (Unqualified unqualified) = Pretty.text $ Text.unpack unqualified

instance Pretty GeneralName where
  pPrint name = case name of
    QualifiedName qualified -> pPrint qualified
    UnqualifiedName unqualified -> pPrint unqualified
    LocalName i -> "local." Pretty.<> Pretty.int i
    IntrinsicName intrinsic -> pPrint intrinsic

instance Pretty Intrinsic where
  pPrint _ = error "TODO: pretty-print intrinsic names"

-- FIXME: Real instance.
instance Pretty Synonym where
  pPrint _ = "synonym"

instance Pretty Signature where
  pPrint signature = case signature of
    ApplicationSignature a b _ -> Pretty.parens $ pPrint a Pretty.<+> pPrint b
    FunctionSignature as bs es _ -> Pretty.parens $ Pretty.hsep $
      [ prettyList $ map pPrint as
      , "->"
      , prettyList $ map pPrint bs
      ] ++ map ((Pretty.char '+' Pretty.<>) . pPrint) es
    QuantifiedSignature names type_ _
      -> (prettyAngles $ prettyList $ map prettyVar names)
        Pretty.<+> pPrint type_
      where

      prettyVar :: (Unqualified, Kind, Origin) -> Pretty.Doc
      prettyVar (name, kind, _) = case kind of
        Value -> pPrint name
        Stack -> pPrint name Pretty.<> "..."
        Effect -> Pretty.char '+' Pretty.<> pPrint name
        _ -> error "quantified signature contains variable of invalid kind"

    SignatureVariable name _ -> pPrint name
    StackFunctionSignature r as s bs es _ -> Pretty.parens $ Pretty.hsep
      $ (pPrint r Pretty.<> "...")
      : map pPrint as ++ ["->"]
      ++ ((pPrint s Pretty.<> "...") : map pPrint bs)
      ++ map ((Pretty.char '+' Pretty.<>) . pPrint) es

instance Pretty Type where
  pPrint type_ = case type_ of
    a :@ b -> pPrint a Pretty.<+> pPrint b
    TypeConstructor (Constructor name) -> pPrint name
    TypeVar var -> pPrint var
    TypeConstant var -> pPrint var
    Forall var a -> prettyAngles (pPrint var)
      Pretty.<+> Pretty.parens (pPrint a)

instance Pretty Kind where
  pPrint kind = case kind of
    Value -> "value"
    Stack -> "stack"
    Label -> "label"
    Effect -> "effect"
    a :-> b -> Pretty.parens $ Pretty.hsep
      [pPrint a, "->", pPrint b]

instance Pretty Var where
  pPrint (Var (TypeId i) kind) = Pretty.parens $ Pretty.hcat
    [ Pretty.char 'T'
    , Pretty.int i
    , Pretty.char ':'
    , pPrint kind
    ]

prettyAngles :: Pretty.Doc -> Pretty.Doc
prettyAngles doc = Pretty.hcat [Pretty.char '<', doc, Pretty.char '>']

prettyList :: [Pretty.Doc] -> Pretty.Doc
prettyList = Pretty.hcat . intersperse ", "




--------------------------------------------------------------------------------
-- K Monad
--------------------------------------------------------------------------------




newtype K a = K { runK :: Env -> IO (Maybe a) }

data Env = Env { envContext :: [Report], envReports :: IORef [[Report]] }

data Report = Report !Origin !Pretty.Doc

instance Functor K where
  fmap f (K ax) = K (fmap (fmap f) . ax)

instance Applicative K where
  pure = K . const . return . Just
  K af <*> K ax = K $ \env -> do
    mf <- af env
    mx <- ax env
    return $ case (mf, mx) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just f, Just x) -> Just (f x)

instance Monad K where
  return = K . const . return . Just
  K ax >>= f = K $ \env -> do
    mx <- ax env
    case mx of
      Nothing -> return Nothing
      Just x -> runK (f x) env
  -- FIXME: Use better origin.
  fail = (>> halt) . report . Report Anywhere . Pretty.text

instance MonadFix K where
  mfix k = K $ \env -> do
    m <- newEmptyMVar
    a <- unsafeInterleaveIO (takeMVar m)
    mx <- runK (k a) env
    case mx of
      Nothing -> return Nothing
      Just x -> do
        putMVar m x
        return (Just x)

instance MonadIO K where
  liftIO = K . const . fmap Just

checkpoint :: K ()
checkpoint = K $ \env -> do
  errors <- readIORef (envReports env)
  return (if null errors then Just () else Nothing)

report :: Report -> K ()
report r = K $ \ env -> Just
  <$> modifyIORef (envReports env) ((r : envContext env) :)

halt :: K a
halt = K (const (return Nothing))

while :: Report -> K a -> K a
while prefix action = K $ \ env
  -> runK action env { envContext = prefix : envContext env }

showReport :: Report -> String
showReport (Report origin message)
  = showOriginPrefix origin ++ Pretty.render message

showOriginPrefix :: Origin -> String
showOriginPrefix (Range a b) = concat
  $ [Parsec.sourceName a, ":", show al, ".", show ac, "-"]
  ++ (if al == bl then [show bc] else [show bl, ".", show bc])
  ++ [": "]
  where
  al = Parsec.sourceLine a
  bl = Parsec.sourceLine b
  ac = Parsec.sourceColumn a
  bc = Parsec.sourceColumn b - 1
showOriginPrefix _ = "<unknown location>: "
