{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&), first)
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable (foldlM, foldrM)
import Data.Function
import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts (IsString(..), groupWith)
import Numeric
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import Text.Parsec (Column, ParsecT, SourcePos, (<?>))
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
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
      forM_ paragraphs $ mapM_ (hPutStrLn stderr . showReport) . reverse
      exitFailure
    Just program -> putStrLn $ Pretty.render $ pPrint program




--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

-- The compiler is a straightforward pipeline. At each stage, errors and
-- warnings ("reports") are accumulated, and reported to the programmer at the
-- next checkpoint.

compile :: [FilePath] -> K Program
compile paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM readFileUtf8 paths

-- They are lexed into a stream of tokens.

  tokenized <- zipWithM tokenize paths sources
  checkpoint

-- Next, the layout rule is applied to desugar indentation-based syntax, so that
-- the parser can find the ends of blocks without checking the indentation of
-- tokens.

  laidout <- zipWithM layout paths tokenized
  checkpoint

-- We parse the token stream as a series of top-level program elements.

  parsed <- mconcat <$> zipWithM parse paths laidout
  checkpoint

-- Datatype definitions are desugared into regular definitions, so that name
-- resolution can find their names.

  dataDesugared <- desugarDataDefinitions parsed

-- Name resolution rewrites unqualified names into fully qualified names, so
-- that it's evident from a name which program element it refers to.

  resolved <- resolveNames dataDesugared
  checkpoint

-- After names have been resolved, the precedences of operators are known, so
-- infix operators can be desugared into postfix syntax.

  postfix <- desugarInfix resolved
  checkpoint

-- In addition, now that we know which names refer to local variables,
-- quotations can be rewritten into closures that explicitly capture the
-- variables they use from the enclosing scope.

  let scoped = scope postfix

-- Finally we can infer and check the types of all definitions.

  inferred <- inferTypes scoped
  checkpoint

-- Knowing the inferred types of all quotations in the program, we can now lift
-- them into top-level definitions.

  desugared <- desugarDefinitions inferred

-- With fully desugared definitions, we can now make generic definitions
-- explicitly indicate the scalar type variables that must be instantiated when
-- generating specializations.

  let quantified = quantifyTerms desugared

-- Also, we now have enough type information to make calls to the destructors
-- and copy constructors of locals explicit.

  let linear = linearize quantified

-- Now we can go through all definitions in the program, starting from the main
-- entry point, and collect specializations of generic definitions.

  instantiated <- collectInstantiations emptyTypeEnv linear
  -- generate instance declarations
  --   (so inference can know what's available in a precompiled module)
  -- generate code
  return instantiated
  where

  readFileUtf8 :: FilePath -> IO Text
  readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile




--------------------------------------------------------------------------------
-- Tokenization
--------------------------------------------------------------------------------

data Token
  = AngleToken !Origin !Indent                   -- > See note [Angle Brackets].
  | ArrowToken !Origin !Indent                   -- ->
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
  | InstanceToken !Origin !Indent                -- instance
  | IntegerToken !Integer !Base !Origin !Indent  -- 1 0b1 0o1 0x1
  | LayoutToken !Origin !Indent                  -- :
  | MatchToken !Origin !Indent                   -- match
  | OperatorToken !Unqualified !Origin !Indent   -- +
  | ReferenceToken !Origin !Indent               -- \
  | SynonymToken !Origin !Indent                 -- synonym
  | TextToken !Text !Origin !Indent              -- "..."
  | TraitToken !Origin !Indent                   -- trait
  | VectorBeginToken !Origin !Indent             -- [
  | VectorEndToken !Origin !Indent               -- ]
  | VocabToken !Origin !Indent                   -- vocab
  | VocabLookupToken !Origin !Indent             -- ::
  | WordToken !Unqualified !Origin !Indent       -- word

-- Minor hack because Parsec requires 'Show'.
instance Show Token where
  show = Pretty.render . pPrint

-- A token origin.
data Origin
  = Range
    { rangeBegin :: !SourcePos
    , rangeEnd :: !SourcePos
    }
  | Anywhere
  deriving (Show)

instance Eq Origin where
  Range a b == Range c d = (a, b) == (c, d)
  _ == Anywhere = True
  Anywhere == _ = True

type Indent = Maybe Column

data Layoutness = Layout | Nonlayout
  deriving (Show)

data Base = Binary | Octal | Decimal | Hexadecimal
  deriving (Show)

----------------------------------------

tokenize :: FilePath -> Text -> K [Token]
tokenize path text = case Parsec.runParser fileTokenizer 1 path text of
  Left parseError -> do
    report $ reportParseError parseError
    halt
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
    in Parsec.choice
      [ do
        name <- alphanumeric
        return $ case name of
          "case" -> CaseToken
          "data" -> DataToken
          "define" -> DefineToken
          "elif" -> ElifToken
          "else" -> ElseToken
          "false" -> BooleanToken False
          "if" -> IfToken
          "infix" -> InfixToken
          "instance" -> InstanceToken
          "match" -> MatchToken
          "synonym" -> SynonymToken
          "trait" -> TraitToken
          "true" -> BooleanToken True
          "vocab" -> VocabToken
          _ -> WordToken (Unqualified name)

-- See note [Angle Brackets].

      , OperatorToken ">"
        <$ Parsec.try (Parsec.char '>' <* Parsec.notFollowedBy symbol)
      , AngleToken <$ Parsec.char '>'
      , OperatorToken . Unqualified . Text.pack <$> Parsec.many1 symbol
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
  AngleToken _ _        == AngleToken _ _        = True
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
  InstanceToken _ _     == InstanceToken _ _     = True
  IntegerToken a _ _ _  == IntegerToken b _ _ _  = a == b
  LayoutToken _ _       == LayoutToken _ _       = True
  MatchToken _ _        == MatchToken _ _        = True
  OperatorToken a _ _   == OperatorToken b _ _   = a == b
  ReferenceToken _ _    == ReferenceToken _ _    = True
  SynonymToken _ _      == SynonymToken _ _      = True
  TextToken a _ _       == TextToken b _ _       = a == b
  TraitToken _ _        == TraitToken _ _        = True
  VectorBeginToken _ _  == VectorBeginToken _ _  = True
  VectorEndToken _ _    == VectorEndToken _ _    = True
  VocabToken _ _        == VocabToken _ _        = True
  VocabLookupToken _ _  == VocabLookupToken _ _  = True
  WordToken a _ _       == WordToken b _ _       = a == b
  _                     == _                     = False

tokenOrigin :: Token -> Origin
tokenOrigin token = case token of
  AngleToken origin _ -> origin
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
  InstanceToken origin _ -> origin
  IntegerToken _ _ origin _ -> origin
  LayoutToken origin _ -> origin
  MatchToken origin _ -> origin
  OperatorToken _ origin _ -> origin
  ReferenceToken origin _ -> origin
  SynonymToken origin _ -> origin
  TextToken _ origin _ -> origin
  TraitToken origin _ -> origin
  VectorBeginToken origin _ -> origin
  VectorEndToken origin _ -> origin
  VocabToken origin _ -> origin
  VocabLookupToken origin _ -> origin
  WordToken _ origin _ -> origin

tokenIndent :: Token -> Indent
tokenIndent token = case token of
  AngleToken _ indent -> indent
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
  InstanceToken _ indent -> indent
  IntegerToken _ _ _ indent -> indent
  LayoutToken _ indent -> indent
  MatchToken _ indent -> indent
  OperatorToken _ _ indent -> indent
  ReferenceToken _ indent -> indent
  SynonymToken _ indent -> indent
  TextToken _ _ indent -> indent
  TraitToken _ indent -> indent
  VectorBeginToken _ indent -> indent
  VectorEndToken _ indent -> indent
  VocabToken _ indent -> indent
  VocabLookupToken _ indent -> indent
  WordToken _ _ indent -> indent




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
    Left parseError -> do
      report $ reportParseError parseError
      halt
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
    firstToken <- Parsec.lookAhead (tokenSatisfy validFirst)
      <?> "token indented no less than start of layout block"
    let
      firstOrigin = rangeBegin (tokenOrigin firstToken)
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
  , fragmentTraits :: [Trait]
  } deriving (Show)

instance Monoid Fragment where
  mempty = Fragment
    { fragmentDataDefinitions = []
    , fragmentDefinitions = []
    , fragmentOperators = []
    , fragmentSynonyms = []
    , fragmentTraits = []
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
    , fragmentTraits
      = fragmentTraits a ++ fragmentTraits b
    }

data Element
  = DataDefinitionElement !DataDefinition
  | DefinitionElement !Definition
  | OperatorElement !Operator
  | SynonymElement !Synonym
  | TermElement !Term
  | TraitElement !Trait

data Trait = Trait
  { traitName :: !Qualified
  , traitOrigin :: !Origin
  , traitSignature :: !Signature
  } deriving (Show)

data Definition = Definition
  { definitionBody :: !Term
  , definitionFixity :: !Fixity
  , definitionInstance :: !DefinitionInstance
  , definitionName :: !Qualified
  , definitionOrigin :: !Origin
  , definitionSignature :: !Signature
  } deriving (Show)

data DefinitionInstance = WordDefinition | InstanceDefinition
  deriving (Show)

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
  = Call !(Maybe Type) !Fixity !GeneralName [Type] !Origin         -- f
  | Compose !(Maybe Type) !Term !Term                              -- e1 e2
  | Drop !(Maybe Type) !Origin                                     -- drop
  | Generic !TypeId !Term !Origin                                  -- Λx. e
  | Group !Term                                                    -- (e)
  | Identity !(Maybe Type) !Origin                                 --
  | If !(Maybe Type) !Term !Term !Origin                           -- if { e1 } else { e2 }
  | Intrinsic !(Maybe Type) !Intrinsic !Origin                     -- .add.int
  | Lambda !(Maybe Type) !Unqualified !(Maybe Type) !Term !Origin  -- → x; e
  | Match !(Maybe Type) [Case] !(Maybe Else) !Origin               -- match { case C {...}... else {...} }
  | New !(Maybe Type) !ConstructorIndex !Origin                    -- new.n
  | NewClosure !(Maybe Type) !Int !Origin                          -- new.closure.n
  | NewVector !(Maybe Type) !Int !Origin                           -- new.vec.n
  | Push !(Maybe Type) !Value !Origin                              -- push v
  | Swap !(Maybe Type) !Origin                                     -- swap
  deriving (Show)

data Value
  = Boolean !Bool
  | Character !Char
  | Closed !ClosureIndex
  | Closure [Closed] !Term
  | Float !Double
  | Integer !Integer
  | Local !LocalIndex
  | Name !Qualified
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
    Left parseError -> do
      report $ reportParseError parseError
      halt
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
    TraitElement x -> acc
      { fragmentTraits = x : fragmentTraits acc }
    TermElement x -> acc
      { fragmentDefinitions
        = case findIndex matching $ fragmentDefinitions acc of
          Just index -> case splitAt index $ fragmentDefinitions acc of
            (a, existing : b) -> a ++ existing { definitionBody
              = composeUnderLambda (definitionBody existing) x } : b
            _ -> error "cannot find main definition"
          Nothing -> Definition
            { definitionBody = x
            , definitionFixity = Postfix
            , definitionInstance = WordDefinition
            , definitionName = Qualified globalVocabulary "main"
            , definitionOrigin = termOrigin x
            , definitionSignature = FunctionSignature [] []
              [QualifiedName (Qualified globalVocabulary "io")] $ termOrigin x
            } : fragmentDefinitions acc
      }
      where
      matching = (== Qualified globalVocabulary "main") . definitionName

-- In top-level code, we want local variable bindings to remain in scope even
-- when separated by other top-level program elements, e.g.:
--
--     1 -> x;
--     define f (int -> int) { (+ 1) }
--     x say  // should work
--
-- As such, when composing top-level code, we extend the scope of lambdas to
-- include subsequent expressions.

      composeUnderLambda :: Term -> Term -> Term
      composeUnderLambda (Lambda type_ name varType body origin) term
        = Lambda type_ name varType (composeUnderLambda body term) origin
      composeUnderLambda a b = Compose Nothing a b

vocabularyParser :: Parser [Element]
vocabularyParser = (<?> "vocabulary definition") $ do
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

-- See note [Angle Brackets].

angledParser :: Parser a -> Parser a
angledParser = Parsec.between (parserMatchOperator "<")
  (parserMatchOperator ">" <|> parserMatch AngleToken)

bracketedParser :: Parser a -> Parser a
bracketedParser = Parsec.between
  (parserMatch VectorBeginToken) (parserMatch VectorEndToken)

nameParser :: Parser (GeneralName, Fixity)
nameParser = (<?> "name") $ do
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
unqualifiedNameParser = (<?> "unqualified name")
  $ wordNameParser <|> operatorNameParser

wordNameParser :: Parser Unqualified
wordNameParser = (<?> "word name") $ parseOne $ \ token -> case token of
  WordToken name _ _ -> Just name
  _ -> Nothing

operatorNameParser :: Parser Unqualified
operatorNameParser = (<?> "operator name") $ do
  -- Rihtlice hi sind Angle gehatene, for ðan ðe hi engla wlite habbað.
  angles <- many $ parseOne $ \ token -> case token of
    AngleToken{} -> Just ">"
    _ -> Nothing
  rest <- parseOne $ \ token -> case token of
    OperatorToken (Unqualified name) _ _ -> Just name
    _ -> Nothing
  return $ Unqualified $ Text.concat $ angles ++ [rest]

parseOne :: (Token -> Maybe a) -> Parser a
parseOne = Parsec.tokenPrim show advance
  where
  advance :: SourcePos -> t -> [Token] -> SourcePos
  advance _ _ (token : _) = rangeBegin $ tokenOrigin token
  advance sourcePos _ _ = sourcePos

elementParser :: Parser Element
elementParser = (<?> "top-level program element") $ Parsec.choice
  [ DataDefinitionElement <$> dataDefinitionParser
  , DefinitionElement <$> (basicDefinitionParser <|> instanceParser)
  , OperatorElement <$> operatorParser
  , SynonymElement <$> synonymParser
  , TraitElement <$> traitParser
  , do
    origin <- getOrigin
    TermElement . compose origin <$> Parsec.many1 termParser
  ]

synonymParser :: Parser Synonym
synonymParser = (<?> "synonym definition") $ do
  origin <- getOrigin <* parserMatch_ SynonymToken
  from <- Qualified <$> Parsec.getState
    <*> (wordNameParser <|> operatorNameParser)
  (to, _) <- nameParser
  return $ Synonym from to origin

operatorParser :: Parser Operator
operatorParser = (<?> "operator definition") $ do
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
  fields <- (<?> "constructor fields") $ Parsec.option [] $ groupedParser
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
    [ do
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
  sideEffects <- (<?> "side effect labels")
    $ many $ parserMatchOperator "+" *> (fst <$> nameParser)
  return (stackEffect sideEffects origin)
  where

  stack :: Parser Unqualified
  stack = Parsec.try $ wordNameParser <* parserMatch EllipsisToken

  left, right :: Parser [Signature]
  left = basicTypeParser `Parsec.sepEndBy` comma
  right = typeParser `Parsec.sepEndBy` comma

  comma :: Parser ()
  comma = void $ parserMatch CommaToken

  arrow :: Parser Origin
  arrow = getOrigin <* parserMatch ArrowToken

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  prefix <- Parsec.choice
    [ quantifiedParser $ groupedParser typeParser
    , Parsec.try $ do
      origin <- getOrigin
      (name, fixity) <- nameParser
      -- Must be a word, not an operator, but may be qualified.
      guard $ fixity == Postfix
      return $ SignatureVariable name origin
    , groupedParser typeParser
    ]
  let apply a b = ApplicationSignature a b $ signatureOrigin prefix
  mSuffix <- Parsec.optionMaybe $ fmap concat
    $ Parsec.many1 $ typeListParser basicTypeParser
  return $ case mSuffix of
    Just suffix -> foldl' apply prefix suffix
    Nothing -> prefix

quantifierParser :: Parser [(Unqualified, Kind, Origin)]
quantifierParser = typeListParser var
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

typeListParser :: Parser a -> Parser [a]
typeListParser element = angledParser
  $ element `Parsec.sepEndBy1` parserMatch CommaToken

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getOrigin
  QuantifiedSignature <$> quantifierParser <*> thing <*> pure origin

traitParser :: Parser Trait
traitParser = (<?> "trait declaration") $ do
  origin <- getOrigin <* parserMatch TraitToken
  suffix <- Parsec.choice
    [wordNameParser, operatorNameParser] <?> "trait name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  signature <- signatureParser
  return Trait
    { traitName = name
    , traitOrigin = origin
    , traitSignature = signature
    }

basicDefinitionParser :: Parser Definition
basicDefinitionParser = (<?> "word definition")
  $ definitionParser DefineToken WordDefinition

instanceParser :: Parser Definition
instanceParser = (<?> "instance definition")
  $ definitionParser InstanceToken InstanceDefinition

definitionParser
  :: (Origin -> Indent -> Token) -> DefinitionInstance -> Parser Definition
definitionParser keyword instance_ = do
  origin <- getOrigin <* parserMatch keyword
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
    , definitionInstance = instance_
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
termParser = (<?> "expression") $ do
  origin <- getOrigin
  Parsec.choice
    [ Parsec.try (uncurry (Push Nothing) <$> parseOne toLiteral <?> "literal")
    , do
      (name, fixity) <- nameParser
      return (Call Nothing fixity name [] origin)
    , Parsec.try sectionParser
    , Parsec.try groupParser <?> "parenthesized expression"
    , vectorParser
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
      let call = Call Nothing Postfix (UnqualifiedName function) [] origin
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
        , Call Nothing Postfix (UnqualifiedName function) [] origin
        ]
    ]

  vectorParser :: Parser Term
  vectorParser = (<?> "vector literal") $ do
    vectorOrigin <- getOrigin
    elements <- bracketedParser
      $ termParser `Parsec.sepEndBy` parserMatch CommaToken
    return $ compose vectorOrigin $ elements
      ++ [NewVector Nothing (length elements) vectorOrigin]

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
  ifParser = (<?> "if-else expression") $ do
    ifOrigin <- getOrigin <* parserMatch IfToken
    mCondition <- Parsec.optionMaybe groupParser <?> "condition"
    ifBody <- blockParser
    elifs <- many $ do
      origin <- getOrigin <* parserMatch ElifToken
      condition <- groupParser <?> "condition"
      body <- blockParser
      return (condition, body, origin)
    else_ <- Parsec.option (Identity Nothing ifOrigin)
      $ parserMatch ElseToken *> blockParser
    let
      desugarCondition (condition, body, origin) acc
        = compose ifOrigin [condition, If Nothing body acc origin]
    return $ foldr desugarCondition else_
      $ (fromMaybe (Identity Nothing ifOrigin) mCondition, ifBody, ifOrigin)
      : elifs

  blockValue :: Parser Value
  blockValue = (<?> "quotation") $ do
    origin <- getOrigin
    let
      reference = Call Nothing Postfix
        <$> (parserMatch ReferenceToken *> (fst <$> nameParser))
        <*> pure []
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
lambdaNamesParser = lambdaName `Parsec.sepEndBy1` parserMatch CommaToken
  where
  lambdaName = do
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
    (\ name -> Lambda Nothing name Nothing acc nameOrigin)
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
  Call _ _ _ _ origin -> origin
  Compose _ a _ -> termOrigin a
  Drop _ origin -> origin
  Generic _ _ origin -> origin
  Group a -> termOrigin a
  Identity _ origin -> origin
  If _ _ _ origin -> origin
  Intrinsic _ _ origin -> origin
  Lambda _ _ _ _ origin -> origin
  New _ _ origin -> origin
  NewClosure _ _ origin -> origin
  NewVector _ _ origin -> origin
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




-- Note [Angle Brackets]:
--
-- Since we separate the passes of tokenization and parsing, we are faced with a
-- classic ambiguity between angle brackets as used in operator names such as
-- '>>' and '>=', and as used in nested type argument lists such as
-- 'vector<vector<T>>'.
--
-- Our solution is to parse a greater-than character as an 'angle' token if it
-- was immediately followed by a symbol character in the input with no
-- intervening whitespace. This is enough information for the parser to
-- disambiguate the intent:
--
--   • When parsing an expression, it joins a sequence of angle tokens and
--     an operator token into a single operator token.
--
--   • When parsing a signature, it treats them separately.
--
-- You may ask why we permit this silly ambiguity in the first place. Why not
-- merge the passes of tokenization and parsing, or use a different bracketing
-- character such as '[]' for type argument lists?
--
-- We separate tokenization and parsing for the sake of tool support: it's
-- simply easier to provide token-accurate source locations when we keep track
-- of source locations at the token level, and it's easier to provide a list of
-- tokens to external tools (e.g., for syntax highlighting) if we already have
-- such a list at hand.
--
-- The reason for the choice of bracketing character is for the sake of
-- compatibility with C++ tools. When setting a breakpoint in GDB, for example,
-- it's nice to be able to type:
--
--     break foo::bar<int>
--
-- And for this to refer to the Kitten definition 'foo::bar<int>' precisely,
-- rather than to some syntactic analogue such as 'foo.bar[int]'. The modest
-- increase in complexity of implementation is justified by fostering a better
-- experience for people.




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

instance Hashable Qualified where
  hashWithSalt s (Qualified qualifier unqualified)
    = hashWithSalt s (0 :: Int, qualifier, unqualified)

data Qualifier = Qualifier [Text]
  deriving (Eq, Ord, Show)

instance Hashable Qualifier where
  hashWithSalt s (Qualifier parts)
    = hashWithSalt s (0 :: Int, Text.concat parts)

data Unqualified = Unqualified Text
  deriving (Eq, Ord, Show)

instance Hashable Unqualified where
  hashWithSalt s (Unqualified name) = hashWithSalt s (0 :: Int, name)

instance IsString Unqualified where
  fromString = Unqualified . Text.pack

type LocalIndex = Int

data Intrinsic
  = AddIntrinsic
  | MagicIntrinsic
  deriving (Eq, Ord, Show)

type Resolved a = StateT [Unqualified] K a




-- Name resolution is responsible for rewriting unqualified calls to definitions
-- into fully qualified calls.

resolveNames :: Fragment -> K Fragment
resolveNames fragment = do
  reportDuplicateDefinitions
    (Set.fromList $ map traitName $ fragmentTraits fragment)
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
      Call _ fixity name params origin -> Call Nothing fixity
        <$> resolveDefinitionName vocabulary name origin
        <*> pure params <*> pure origin
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
      Lambda _ name _ term origin -> withLocal name
        $ Lambda Nothing name Nothing <$> recur term <*> pure origin
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
      NewClosure{} -> return unresolved
      NewVector{} -> return unresolved
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
    -- FIXME: Maybe should be a GeneralName and require resolution.
    Name{} -> return value
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

  resolveDefinitionName = resolveName "word" resolveLocal isDefined
    where
    isDefined = flip Set.member defined
    defined = Set.fromList $ map definitionName $ fragmentDefinitions fragment
    resolveLocal _ index = return $ LocalName index

  resolveTypeName = resolveName "type" resolveLocal isDefined
    where
    isDefined = flip Set.member defined
    defined = Set.fromList $ map dataName $ fragmentDataDefinitions fragment
    resolveLocal name _ = return $ UnqualifiedName name

  resolveName
    :: Pretty.Doc -> (Unqualified -> LocalIndex -> Resolved GeneralName)
    -> (Qualified -> Bool) -> Qualifier -> GeneralName -> Origin
    -> Resolved GeneralName
  resolveName thing resolveLocal isDefined vocabulary name origin = case name of

-- An unqualified name may refer to a local, a name in the current vocabulary,
-- or a name in the global scope, respectively.

    UnqualifiedName unqualified -> do
      mLocalIndex <- gets (elemIndex unqualified)
      case mLocalIndex of
        Just index -> resolveLocal unqualified index
        Nothing -> do
          let qualified = Qualified vocabulary unqualified
          if isDefined qualified then return (QualifiedName qualified) else do
            let global = Qualified globalVocabulary unqualified
            if isDefined global then return (QualifiedName global) else do
              lift $ report $ Report Error [Item origin
                ["the unqualified", thing, pQuote name, "is not defined"]]
              return name

-- A qualified name must be fully qualified, and may refer to an intrinsic or a
-- definition, respectively.

    QualifiedName qualified -> case intrinsicFromName qualified of
      Just intrinsic -> return (IntrinsicName intrinsic)
      Nothing -> do
        if isDefined qualified then return name else do
          let
            qualified' = case (vocabulary, qualifierName qualified) of
              (Qualifier prefix, Qualifier suffix)
                -> Qualified (Qualifier (prefix ++ suffix))
                  $ unqualifiedName qualified
          if isDefined qualified' then return $ QualifiedName qualified' else do
            lift $ report $ Report Error [Item origin
              ["the qualified", thing, pQuote name, "is not defined"]]
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

reportDuplicateDefinitions :: Set Qualified -> [(Qualified, Origin)] -> K ()
reportDuplicateDefinitions generic = mapM_ reportDuplicate . groupWith fst
  where

  reportDuplicate :: [(Qualified, Origin)] -> K ()
  reportDuplicate defs = case defs of
    [] -> return ()
    [_] -> return ()
    ((name, origin) : duplicates) -> if name `Set.member` generic
      then return ()
      else report $ Report Error
        $ Item origin ["I found multiple definitions of", pQuote name]
        : map (\ duplicateOrigin -> Item duplicateOrigin ["here"])
          (origin : map snd duplicates)
        ++ [Item origin
          ["Did you mean to declare it as a trait?"]]

intrinsicFromName :: Qualified -> Maybe Intrinsic
intrinsicFromName name = case name of
  Qualified qualifier (Unqualified "+")
    | qualifier == globalVocabulary -> Just AddIntrinsic
  Qualified qualifier (Unqualified "magic")
    | qualifier == globalVocabulary -> Just MagicIntrinsic
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
    desugared <- desugarTerms' $ definitionBody definition
    return definition { definitionBody = desugared }
    where

    desugarTerms :: [Term] -> K Term
    desugarTerms terms = do
      terms' <- mapM desugarTerm terms
      let
        expression' = infixExpression <* Parsec.eof
        infixExpression = do
          desugaredTerms <- many $ expression <|> lambda
          let
            origin = case desugaredTerms of
              term : _ -> termOrigin term
              _ -> definitionOrigin definition
          return $ compose origin desugaredTerms
      case Parsec.runParser expression' () "" terms' of
        Left parseError -> do
          report $ reportParseError parseError
          let
            origin = case terms of
              term : _ -> termOrigin term
              _ -> definitionOrigin definition
          return $ compose origin terms
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
      Lambda _ name _ body origin -> Lambda Nothing name Nothing
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
      NewClosure{} -> return term
      NewVector{} -> return term
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
      Name{} -> return value
      Quotation body -> Quotation <$> desugarTerms' body
      Text{} -> return value

    desugarTerms' :: Term -> K Term
    desugarTerms' = desugarTerms . decompose

  expression :: Rewriter Term
  expression = Expression.buildExpressionParser operatorTable operand
    where
    operand = (<?> "operand") $ do
      origin <- getOrigin
      results <- Parsec.many1 $ termSatisfy $ \ term -> case term of
        Call _ Infix _ _ _ -> False
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
    Call _ Infix name' _ origin | name == name' -> Just (binary name origin)
    _ -> Nothing

  binary :: GeneralName -> Origin -> Term -> Term -> Term
  binary name origin x y = compose origin
    [x, y, Call Nothing Postfix name [] origin]

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




newtype LambdaIndex = LambdaIndex Int

desugarDataDefinitions :: Fragment -> K Fragment
desugarDataDefinitions fragment = do
  definitions <- fmap concat $ mapM desugarDataDefinition
    $ fragmentDataDefinitions fragment
  return fragment { fragmentDefinitions
    = fragmentDefinitions fragment ++ definitions }
  where

  desugarDataDefinition :: DataDefinition -> K [Definition]
  desugarDataDefinition definition = mapM (uncurry desugarConstructor)
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
        , definitionInstance = WordDefinition
        , definitionName = Qualified qualifier $ constructorName constructor
        , definitionOrigin = origin
        , definitionSignature = constructorSignature
        }
      where
      origin = constructorOrigin constructor
      qualifier = qualifierFromName $ dataName definition




----------------------------------------
-- Quotation Desugaring
----------------------------------------




desugarDefinitions :: Program -> K Program
desugarDefinitions program = do
  definitions <- HashMap.fromList . concat <$> mapM (uncurry desugarDefinition)
    (HashMap.toList $ programDefinitions program)
  return program { programDefinitions = definitions }
  where

  desugarDefinition
    :: (Qualified, Type) -> Term -> K [((Qualified, Type), Term)]
  desugarDefinition (name, type_) body = do
    (body', lifted) <- desugarTerm (qualifierFromName name) body
    return $ ((name, type_), body') : lifted

  desugarTerm :: Qualifier -> Term -> K (Term, [((Qualified, Type), Term)])
  desugarTerm qualifier = flip evalStateT (LambdaIndex 0) . go
    where

    go :: Term -> StateT LambdaIndex K (Term, [((Qualified, Type), Term)])
    go term = case term of
      Call{} -> done
      Compose type_ a b -> do
        (a', as) <- go a
        (b', bs) <- go b
        return (Compose type_ a' b', as ++ bs)
      Drop{} -> done
      Generic{} -> error
        "generic expression should not appear before desugaring"
      Group{} -> error "group should not appear after infix desugaring"
      Identity{} -> done
      If type_ a b origin -> do
        (a', as) <- go a
        (b', bs) <- go b
        return (If type_ a' b' origin, as ++ bs)
      Intrinsic{} -> done
      Lambda type_ name varType a origin -> do
        (a', as) <- go a
        return (Lambda type_ name varType a' origin, as)
      Match type_ cases mElse origin -> do
        (cases', as) <- flip mapAndUnzipM cases
          $ \ (Case name a caseOrigin) -> do
            (a', xs) <- go a
            return (Case name a' caseOrigin, xs)
        (mElse', bs) <- case mElse of
          Just (Else a elseOrigin) -> do
            (a', xs) <- go a
            return (Just (Else a' elseOrigin), xs)
          Nothing -> return (Nothing, [])
        return (Match type_ cases' mElse' origin, concat as ++ bs)
      New{} -> done
      NewClosure{} -> done
      NewVector{} -> done
      -- FIXME: Should be Closure, not Quotation.
      Push _type (Closure closed a) origin -> do
        (a', as) <- go a
        LambdaIndex index <- get
        let
          name = Qualified qualifier
            $ Unqualified $ Text.pack $ "lambda" ++ show index
        put $ LambdaIndex $ succ index
        let
          deducedType = case termType a of
            Just t -> t
            Nothing -> error "cannot lift quotation before type inference"
          type_ = foldr (uncurry ((Forall origin .) . Var)) deducedType
            $ Map.toList $ freeTvks deducedType
        return
          ( compose origin $ map pushClosed closed ++
            -- FIXME: What type should be used here?
            [ Push Nothing (Name name) origin
            , NewClosure Nothing (length closed) origin
            ]
          , ((name, type_), a') : as
          )
        where

        pushClosed :: Closed -> Term
        pushClosed name = Push Nothing (case name of
          ClosedLocal index -> Local index
          ClosedClosure index -> Closed index) origin

      Push{} -> done
      Swap{} -> done
      where
      done = return (term, [])




----------------------------------------
-- Linear Desugaring
----------------------------------------




-- Linearization replaces all copies and drops with explicit invocations of the
-- '_::copy' and '_::drop' words. A value is copied if it appears twice or more
-- in its scope; it's dropped if it doesn't appear at all, or if an explicit
-- 'drop' is present due to an ignored local (_). If it only appears once, it is
-- moved, and no special word is invoked.

linearize :: Program -> Program
linearize program = let
  definitions' = HashMap.fromList $ map linearizeDefinition
    $ HashMap.toList $ programDefinitions program
  in program { programDefinitions = definitions' }
  where

  linearizeDefinition
    :: ((Qualified, Type), Term) -> ((Qualified, Type), Term)
  linearizeDefinition ((name, type_), body)
    = ((name, type_), linearizeTerm body)

  linearizeTerm :: Term -> Term
  linearizeTerm = snd . go []
    where

    go :: [Int] -> Term -> ([Int], Term)
    go counts0 term = case term of
      Call{} -> (counts0, term)
      Compose type_ a b -> let
        (counts1, a') = go counts0 a
        (counts2, b') = go counts1 b
        in (counts2, Compose type_ a' b')
      Drop{} -> (counts0, term)
      Generic x body origin -> let
        (counts1, body') = go counts0 body
        in (counts1, Generic x body' origin)
      Group{} -> error "group should not appear after desugaring"
      Identity{} -> (counts0, term)
      If type_ a b origin -> let
        (counts1, a') = go counts0 a
        (counts2, b') = go counts0 b
        in (zipWith max counts1 counts2, If type_ a' b' origin)
      Intrinsic{} -> (counts0, term)
      Lambda type_ x varType body origin -> let
        (n : counts1, body') = go (0 : counts0) body
        varType' = maybe (error "lambda missing variable type") id varType
        body'' = case n of
          0 -> instrumentDrop origin varType' body'
          1 -> body'
          _ -> instrumentCopy varType' body'
        in (counts1, Lambda type_ x varType body'' origin)
      -- FIXME: count usages for each branch & take maximum
      Match type_ cases mElse origin -> let

        (counts1, mElse') = goElse counts0 mElse
        (counts2, cases') = first (map maximum . transpose)
          $ unzip $ map (goCase counts0) cases
        in (zipWith max counts1 counts2, Match type_ cases' mElse' origin)
        where

        goCase :: [Int] -> Case -> ([Int], Case)
        goCase counts (Case name body caseOrigin) = let
          (counts1, body') = go counts body
          in (counts1, Case name body' caseOrigin)

        goElse :: [Int] -> Maybe Else -> ([Int], Maybe Else)
        goElse counts (Just (Else body elseOrigin)) = let
          (counts1, body') = go counts body
          in (counts1, Just (Else body' elseOrigin))
        goElse counts Nothing = (counts, Nothing)

      New{} -> (counts0, term)
      NewClosure{} -> (counts0, term)
      NewVector{} -> (counts0, term)
      Push _ (Local index) _ -> let
        (h, t : ts) = splitAt index counts0
        in (h ++ succ t : ts, term)
      Push _ Closure{} _ -> error
        "pushing of closure should not appear after desugaring"
      Push _ Quotation{} _ -> error
        "pushing of quotation should not appear after desugaring"
      Push{} -> (counts0, term)
      Swap{} -> (counts0, term)

  instrumentDrop :: Origin -> Type -> Term -> Term
  instrumentDrop origin type_ a = compose origin
    [ a
    , Push Nothing (Local 0) origin
    , Call Nothing Postfix
      (QualifiedName (Qualified globalVocabulary "drop")) [type_] origin
    ]

  instrumentCopy :: Type -> Term -> Term
  instrumentCopy varType = go 0
    where

    go :: Int -> Term -> Term
    go n term = case term of
      Call{} -> term
      Compose type_ a b -> Compose type_ (go n a) (go n b)
      Drop{} -> term
      Generic x body origin -> Generic x (go n body) origin
      Group{} -> error "group should not appear after desugaring"
      Identity{} -> term
      If type_ a b origin -> If type_ (go n a) (go n b) origin
      Intrinsic{} -> term
      Lambda type_ name varType' body origin
        -> Lambda type_ name varType' (go (succ n) body) origin
      Match type_ cases mElse origin
        -> Match type_ (map goCase cases) (goElse <$> mElse) origin
        where

        goCase :: Case -> Case
        goCase (Case name body caseOrigin) = Case name (go n body) caseOrigin

        goElse :: Else -> Else
        goElse (Else body elseOrigin) = Else (go n body) elseOrigin

      New{} -> term
      NewClosure{} -> term
      NewVector{} -> term
      Push _ (Local index) origin | index == n
        -> Compose Nothing term $ Call Nothing Postfix
          (QualifiedName (Qualified globalVocabulary "copy")) [varType] origin
      Push{} -> term
      Swap{} -> term




--------------------------------------------------------------------------------
-- Scope Resolution
--------------------------------------------------------------------------------




-- Whereas name resolution is concerned with resolving references to
-- definitions, scope resolution resolves local names to relative (De Bruijn)
-- indices, and converts quotations to use explicit closures.

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
      Call _ _ (LocalName index) _ origin
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
      Lambda _ name _ a origin -> Lambda Nothing name Nothing
        (scopeTerm (mapHead succ stack) a) origin
      Match _ cases mElse origin -> Match Nothing
        (map (\ (Case name a caseOrigin)
          -> Case name (recur a) caseOrigin) cases)
        (fmap (\ (Else a elseOrigin)
          -> Else (recur a) elseOrigin) mElse)
        origin
      New{} -> term
      NewClosure{} -> term
      NewVector{} -> term
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
    Name{} -> value
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
  Lambda _ name _ a origin -> let
    inside env = env
      { scopeStack = mapHead succ (scopeStack env)
      , scopeDepth = succ (scopeDepth env)
      }
    in Lambda Nothing name Nothing
      <$> local inside (captureTerm a) <*> pure origin
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
  NewClosure{} -> return term
  NewVector{} -> return term
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
  Name{} -> return value
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

data Program = Program
  { programDefinitions :: HashMap (Qualified, Type) Term
  , programTraits :: HashMap Qualified Type
  }

emptyProgram :: Program
emptyProgram = Program
  { programDefinitions = HashMap.empty
  , programTraits = HashMap.empty
  }




-- This is the type language. It describes a system of conventional Hindley–
-- Milner types, with type constructors joined by type application, as well as
-- type variables and constants for constraint solving and instance checking,
-- respectively. It syntactically permits higher-ranked quantification, though
-- there are semantic restrictions on this, discussed in the presentation of the
-- inference algorithm. Type variables have explicit kinds.

data Type
  = !Type :@ !Type
  | TypeConstructor !Origin !Constructor
  | TypeVar !Origin !Var
  | TypeConstant !Origin !Var
  | Forall !Origin !Var !Type
 deriving (Show)

instance Eq Type where
  (a :@ b) == (c :@ d) = (a, b) == (c, d)
  TypeConstructor _ a == TypeConstructor _ b = a == b
  TypeVar _ a == TypeVar _ b = a == b
  TypeConstant _ a == TypeConstant _ b = a == b
  Forall _ a b == Forall _ c d = (a, b) == (c, d)
  _ == _ = False

instance Hashable Type where
  hashWithSalt s type_ = case type_ of
    a :@ b -> hashWithSalt s (0 :: Int, a, b)
    TypeConstructor _ a -> hashWithSalt s (1 :: Int, a)
    TypeVar _ a -> hashWithSalt s (2 :: Int, a)
    TypeConstant _ a -> hashWithSalt s (3 :: Int, a)
    Forall _ a b -> hashWithSalt s (4 :: Int, a, b)

typeOrigin :: Type -> Origin
typeOrigin type_ = case type_ of
  a :@ _ -> typeOrigin a
  TypeConstructor origin _ -> origin
  TypeVar origin _ -> origin
  TypeConstant origin _ -> origin
  Forall origin _ _ -> origin

setTypeOrigin :: Origin -> Type -> Type
setTypeOrigin origin = go
  where
  go type_ = case type_ of
    a :@ b -> go a :@ go b
    TypeConstructor _ constructor -> TypeConstructor origin constructor
    TypeVar _ var -> TypeVar origin var
    TypeConstant _ var -> TypeConstant origin var
    Forall _ var t -> Forall origin var $ go t

newtype Constructor = Constructor Qualified
  deriving (Eq, Hashable, Show)

data Var = Var !TypeId !Kind
  deriving (Eq, Show)

instance Hashable Var where
  hashWithSalt s (Var a b) = hashWithSalt s (0 :: Int, a, b)

instance IsString Constructor where
  fromString = Constructor
    . Qualified globalVocabulary . Unqualified . Text.pack

funType :: Origin -> Type -> Type -> Type -> Type
funType origin a b e = TypeConstructor origin "fun" :@ a :@ b :@ e

infixl 1 :@

prodType :: Origin -> Type -> Type -> Type
prodType origin a b = TypeConstructor origin "prod" :@ a :@ b

joinType :: Origin -> Type -> Type -> Type
joinType origin a b = TypeConstructor origin "join" :@ a :@ b




-- Type variables are distinguished by globally unique identifiers. This makes
-- it easier to support capture-avoiding substitution on types.

newtype TypeId = TypeId Int
  deriving (Enum, Eq, Hashable, Ord, Show)




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

instance Hashable Kind where
  hashWithSalt s kind = case kind of
    Value -> hashWithSalt s (0 :: Int)
    Stack -> hashWithSalt s (1 :: Int)
    Label -> hashWithSalt s (2 :: Int)
    Effect -> hashWithSalt s (3 :: Int)
    a :-> b -> hashWithSalt s (4 :: Int, a, b)




-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What is the type of this type variable?
--  • What is the type of this local variable?
--  • What are the types of the current closure?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TypeEnv = TypeEnv
  { tenvTvs :: !(Map TypeId Type)
  , tenvVs :: [Type]
  , tenvClosure :: [Type]
  , tenvSigs :: !(Map Qualified Type)
  , tenvCurrentType :: !(IORef TypeId)
  }

instance Pretty TypeEnv where
  pPrint tenv = Pretty.vcat
    $ map (\ (v, t) -> Pretty.hsep [pPrint v, "~", pPrint t])
    $ Map.toList $ tenvTvs tenv

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv
  { tenvTvs = Map.empty
  , tenvVs = []
  , tenvClosure = []
  , tenvSigs = Map.empty
  , tenvCurrentType = currentTypeId
  }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

freshTv :: TypeEnv -> Origin -> Kind -> K Type
freshTv tenv origin k = TypeVar origin <$> (Var <$> freshTypeId tenv <*> pure k)

freshTypeId :: TypeEnv -> K TypeId
freshTypeId tenv = do
  x <- liftIO $ readIORef $ tenvCurrentType tenv
  liftIO $ writeIORef (tenvCurrentType tenv) $ succ x
  return x




inferTypes :: Fragment -> K Program
inferTypes fragment = do
  let tenv0 = emptyTypeEnv

-- We begin by converting instance definitions into ordinary word definitions.
-- With their mangled names already in the global vocabulary, they will be found
-- first when checking for instantiations during instance collection.

  definitions <- forM (fragmentDefinitions fragment) $ \ definition -> do
    type_ <- typeFromSignature tenv0 $ definitionSignature definition
    let name = definitionName definition
    name' <- case definitionInstance definition of
      InstanceDefinition -> let
        mTrait = find ((name ==) . traitName) $ fragmentTraits fragment
        in case mTrait of
          Nothing -> error "instance refers to a nonexistent trait"
          Just trait -> do
            instanceType <- typeFromSignature tenv0
              $ definitionSignature definition
            traitType <- typeFromSignature tenv0 $ traitSignature trait
            instanceCheck "trait" traitType "instance" instanceType
            (traitType', args, tenv1) <- instantiatePrenex tenv0 traitType
            tenv2 <- unifyType tenv1 instanceType traitType'
            let
              args' = valueKinded $ map (zonkType tenv2) args
              mangled = mangleName name args'
            return $ Qualified globalVocabulary $ Unqualified mangled
      WordDefinition -> return name
    return ((name', type_), definitionBody definition)
  traits <- forM (fragmentTraits fragment) $ \ trait -> do
    type_ <- typeFromSignature tenv0 $ traitSignature trait
    return (traitName trait, type_)
  let
    declaredTypes = Map.union
      (Map.fromList (map fst definitions))
      (Map.fromList traits)
    infer = inferType0 declaredTypes

    go :: (Qualified, Type) -> Term -> K ((Qualified, Type), Term)
    go (name, declaredScheme) term = do
      (term', inferredScheme) <- infer name declaredScheme term
      case find ((name ==) . traitName) $ fragmentTraits fragment of
        Just trait -> do
          traitScheme <- typeFromSignature tenv0 $ traitSignature trait
          instanceCheck "generic" traitScheme "declared" declaredScheme
        Nothing -> return ()
      -- FIXME: Should this be the inferred or declared scheme?
      return ((name, inferredScheme), term')
  definitions' <- mapM (uncurry go) definitions
  return Program
    { programTraits = HashMap.fromList traits
    , programDefinitions = HashMap.fromList definitions'
    }




-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0 :: Map Qualified Type -> Qualified -> Type -> Term -> K (Term, Type)
inferType0 sigs _name declared term = {- while ["inferring the type of", show term] $ -} do
  rec
    (term', t, tenvFinal) <- inferType tenvFinal'
      emptyTypeEnv { tenvSigs = sigs } term
    tenvFinal' <- unifyType tenvFinal t declared
  let zonked = zonkType tenvFinal' t
  let regeneralized = regeneralize tenvFinal' zonked
  instanceCheck "inferred" regeneralized "declared" declared
  return (zonkTerm tenvFinal' term', regeneralized)




-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType :: TypeEnv -> TypeEnv -> Term -> K (Term, Type, TypeEnv)
inferType tenvFinal tenv0 term0
  = {- while ["inferring the type of", show term] $ -} case term0 of

    -- FIXME: Should generic parameters be restricted to none?
    Call _ _fixity name _ origin -> inferCall tenvFinal tenv0 name origin

-- The type of the composition of two expressions is the composition of the
-- types of those expressions.

    Compose _ term1 term2 -> do
      (term1', t1, tenv1) <- inferType' tenv0 term1
      (term2', t2, tenv2) <- inferType' tenv1 term2
      (a, b, e1, tenv3) <- unifyFunction tenv2 t1
      (c, d, e2, tenv4) <- unifyFunction tenv3 t2
      tenv5 <- unifyType tenv4 b c
      tenv6 <- unifyType tenv5 e1 e2
      -- FIXME: Use range origin over whole composition?
      let origin = termOrigin term1
      let type_ = funType origin a d e1
      let type' = zonkType tenvFinal type_
      return (Compose (Just type') term1' term2', type_, tenv6)

    Drop _ origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Effect]
      let type_ = funType origin (prodType origin a b) a e
      let type' = zonkType tenvFinal type_
      return (Drop (Just type') origin, type_, tenv0)

    Generic{} -> error
      "generic expresison should not appear during type inference"
    Group{} -> error
      "group expression should not appear during type inference"

-- The empty program is the identity function on stacks.

    Identity _ origin -> do
      [a, e] <- fresh origin [Stack, Effect]
      let type_ = funType origin a a e
      let type' = zonkType tenvFinal type_
      return (Identity (Just type') origin, type_, tenv0)

-- A conditional expression consumes a Boolean and applies one of its two
-- branches to the remainder of the stack. Note that an 'if' without an 'else'
-- is sugar for an 'if' with an empty (i.e., identity) 'else' branch, and this
-- works out neatly in the types.

    If _ true false origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Effect]
      (true', t1, tenv1) <- {- while ["checking true branch"] $ -}
        inferType' tenv0 true
      (false', t2, tenv2) <- {- while ["checking true branch"] $ -}
        inferType' tenv1 false
      tenv3 <- unifyType tenv2 t1 $ funType origin a b e
      tenv4 <- unifyType tenv3 t2 $ funType origin a b e
      let
        type_ = funType origin
          (prodType origin a $ TypeConstructor origin "bool") b e
        type' = zonkType tenvFinal type_
      return (If (Just type') true' false' origin, type_, tenv4)

    Intrinsic _ name origin -> inferIntrinsic tenvFinal tenv0 name origin

-- A local variable binding in Kitten is in fact a lambda term in the ordinary
-- lambda-calculus sense. We infer the type of its body in the

    Lambda _ name _ term origin -> do
      a <- freshTv tenv0 origin Value
      let oldLocals = tenvVs tenv0
      let localEnv = tenv0 { tenvVs = a : tenvVs tenv0 }
      (term', t1, tenv1) <- inferType tenvFinal localEnv term
      let tenv2 = tenv1 { tenvVs = oldLocals }
      (b, c, e, tenv3) <- unifyFunction tenv2 t1
      let
        type_ = funType origin (prodType origin b a) c e
        type' = zonkType tenvFinal type_
        varType' = zonkType tenvFinal a
      return
        ( Lambda (Just type') name (Just varType') term' origin
        , type_
        , tenv3
        )

    Match _ cases mElse origin -> do
      (cases', caseTypes, tenv1) <- foldrM inferCase' ([], [], tenv0) cases
      (mElse', elseType, tenv2) <- case mElse of
        Just (Else body elseOrigin) -> do
          (body', bodyType, tenv') <- inferType' tenv1 body
          return (Just (Else body' elseOrigin), bodyType, tenv')
        Nothing -> do
          [a, b, e] <- fresh origin [Stack, Stack, Effect]
          let effect = joinType origin (TypeConstructor origin "fail") e
          return (Nothing, funType origin a b effect, tenv1)
      tenv3 <- foldrM (\ type_ tenv -> unifyType tenv elseType type_)
        tenv2 caseTypes
      let type_ = setTypeOrigin origin elseType
      let type' = zonkType tenvFinal type_
      return (Match (Just type') cases' mElse' origin, type_, tenv3)

      where
      inferCase' case_ (cases', types, tenv) = do
        (case', type_, tenv') <- inferCase tenvFinal tenv case_
        return (case' : cases', type_ : types, tenv')

-- A 'new' expression simply tags some fields on the stack, so the most
-- straightforward way to type it is as an unsafe cast. For now, we can rely on
-- the type signature of the desugared data constructor definition to make this
-- type-safe, since only the compiler can generate 'new' expressions.

    New _ constructor origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Effect]
      let type_ = funType origin a b e
      let type' = zonkType tenvFinal type_
      return (New (Just type') constructor origin, type_, tenv0)

-- Unlike with 'new', we cannot simply type a 'new closure' expression as an
-- unsafe cast because we need to know its effect on the stack within the body
-- of a definition. So we type a 'new.closure.x' expression as:
--
--     ∀ρστα̂. ρ × α₀ × … × αₓ × (σ → τ) → ρ × (σ → τ)
--

    NewClosure _ size origin -> do
      [a, b, c, d, e1, e2] <- fresh origin
        [Stack, Value, Stack, Stack, Effect, Effect]
      let
        f = funType origin c d e1
        type_ = funType origin
          (foldl' (prodType origin) a (replicate size b ++ [f]))
          (prodType origin a f)
          e2
        type' = zonkType tenvFinal type_
      return (NewClosure (Just type') size origin, type_, tenv0)

-- This is similar for 'new vector' expressions, which we type as:
--
--     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
--

    NewVector _ size origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Effect]
      let
        type_ = funType origin
          (foldl' (prodType origin) a (replicate size b))
          (prodType origin a (TypeConstructor origin "vector" :@ b))
          e
        type' = zonkType tenvFinal type_
      return (NewVector (Just type') size origin, type_, tenv0)

-- Pushing a value results in a stack with that value on top.

    Push _ value origin -> do
      [a, e] <- fresh origin [Stack, Effect]
      (value', t, tenv1) <- inferValue tenvFinal tenv0 origin value
      let type_ = funType origin a (prodType origin a t) e
      let type' = zonkType tenvFinal type_
      return (Push (Just type') value' origin, type_, tenv1)

    Swap _ origin -> do
      [a, b, c, e] <- fresh origin [Stack, Value, Value, Effect]
      let
        type_ = funType origin
          (prodType origin (prodType origin a b) c)
          (prodType origin (prodType origin a c) b)
          e
      let type' = zonkType tenvFinal type_
      return (Swap (Just type') origin, type_, tenv0)

  where
  inferType' = inferType tenvFinal
  fresh origin = foldrM (\k ts -> (: ts) <$> freshTv tenv0 origin k) []




-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

inferCase :: TypeEnv -> TypeEnv -> Case -> K (Case, Type, TypeEnv)
inferCase tenvFinal tenv0 (Case qualified@(QualifiedName name) body origin) = do
  (body', bodyType, tenv1) <- inferType tenvFinal tenv0 body
  (a1, b1, e1, tenv2) <- unifyFunction tenv1 bodyType
  case Map.lookup name $ tenvSigs tenv2 of
    Just signature -> do
      (a2, b2, e2, tenv3) <- unifyFunction tenv2 signature
      -- Note that we swap the consumption and production of the constructor
      -- to get the type of the deconstructor. The body consumes the fields.
      tenv4 <- unifyType tenv3 a1 a2
      tenv5 <- unifyType tenv4 e1 e2
      let type_ = funType origin b2 b1 e1
      -- FIXME: Should a case be annotated with a type?
      -- let type' = zonkType tenvFinal type_
      return (Case qualified body' origin, type_, tenv5)
    Nothing -> error
      "case constructor missing signature after name resolution"
inferCase _ _ _ = error "case of non-qualified name after name resolution"




inferValue :: TypeEnv -> TypeEnv -> Origin -> Value -> K (Value, Type, TypeEnv)
inferValue tenvFinal tenv0 origin value = case value of
  Boolean{} -> return (value, TypeConstructor origin "bool", tenv0)
  Character{} -> return (value, TypeConstructor origin "char", tenv0)
  Closed index -> return (value, tenvClosure tenv0 !! index, tenv0)
  Closure names term -> do
    let types = map (getClosed tenv0) names
    let oldClosure = tenvClosure tenv0
    let localEnv = tenv0 { tenvClosure = types }
    (term', t1, tenv1) <- inferType tenvFinal localEnv term
    let tenv2 = tenv1 { tenvClosure = oldClosure }
    return (Closure names term', t1, tenv2)
  Float{} -> return (value, TypeConstructor origin "float", tenv0)
  Integer{} -> return (value, TypeConstructor origin "int", tenv0)
  Local index -> return (value, tenvVs tenv0 !! index, tenv0)
  Quotation{} -> error "quotation should not appear during type inference"
  Name{} -> error "raw name should not appear during type inference"
  Text{} -> return
    ( value
    , TypeConstructor origin "vector" :@ TypeConstructor origin "char"
    , tenv0
    )
  where

  getClosed :: TypeEnv -> Closed -> Type
  getClosed tenv name = case name of
    ClosedLocal index -> tenvVs tenv !! index
    ClosedClosure index -> tenvClosure tenv !! index




inferCall
  :: TypeEnv -> TypeEnv -> GeneralName -> Origin -> K (Term, Type, TypeEnv)
inferCall tenvFinal tenv0 (QualifiedName name) origin
  = case Map.lookup name $ tenvSigs tenv0 of
    Just t@Forall{} -> do
      (type_, params, tenv1) <- instantiatePrenex tenv0 t
      let
        type' = setTypeOrigin origin type_
        params' = valueKinded params
        type'' = zonkType tenvFinal type'
        params'' = map (zonkType tenvFinal) params'
      return
        ( Call (Just type'') Postfix (QualifiedName name) params'' origin
        , type'
        , tenv1
        )
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> do
      report $ Report Error [Item origin
        ["I can't find a type signature for the word", pQuote name]]
      halt

inferCall tenvFinal tenv0 name@(IntrinsicName intrinsic) origin
  = case intrinsic of
    AddIntrinsic -> do
      a <- freshTv tenv0 origin Stack
      e <- freshTv tenv0 origin Effect
      let
        type_ = funType origin
          (prodType origin
            (prodType origin a (TypeConstructor origin "int"))
            (TypeConstructor origin "int"))
          (prodType origin a (TypeConstructor origin "int"))
          e
        type' = zonkType tenvFinal type_
      returnCall type_ type'
    MagicIntrinsic -> do
      a <- freshTv tenv0 origin Stack
      b <- freshTv tenv0 origin Stack
      e <- freshTv tenv0 origin Effect
      let
        type_ = funType origin a b e
        type' = zonkType tenvFinal type_
      returnCall type_ type'

  where

  -- FIXME: Generate instantiation.
  returnCall type_ type' = return
    (Call (Just type') Postfix name [] origin, type_, tenv0)


inferCall _ _ _ _ = error "cannot infer type of non-qualified name"




inferIntrinsic
  :: TypeEnv -> TypeEnv -> Intrinsic -> Origin -> K (Term, Type, TypeEnv)
inferIntrinsic _ _ _ _ = return $ error "TODO: infer intrinsic"




typeKind :: Type -> Kind
typeKind t = case t of
  -- TODON'T: hard-code these.
  TypeConstructor _origin constructor -> case constructor of
    Constructor (Qualified qualifier unqualified)
      | qualifier == globalVocabulary -> case unqualified of
        "int" -> Value
        "bool" -> Value
        "char" -> Value
        "float" -> Value
        "text" -> Value
        "fun" -> Stack :-> Stack :-> Effect :-> Value
        "prod" -> Stack :-> Value :-> Stack
        "vec" -> Value :-> Value
        "ptr" -> Value :-> Value
        "unsafe" -> Label
        "pure" -> Label
        "io" -> Label
        "fail" -> Label
        "join" -> Label :-> Effect :-> Effect
        _ -> error "can't infer kind of constructor"
    _ -> error "TODO: infer kinds properly"
  TypeVar _origin (Var _ k) -> k
  TypeConstant _origin (Var _ k) -> k
  Forall _origin _ t' -> typeKind t'
  a :@ _b -> let
    ka = typeKind a
    in case ka of
      _ :-> k -> k
      _ -> error "applying non-constructor to type"




-- Deduces the explicit type of a term.

termType :: Term -> Maybe Type
termType term = case term of
  Call t _ _ _ _ -> t
  Compose t _ _ -> t
  Drop t _ -> t
  Generic _ term' _ -> termType term'
  Group term' -> termType term'
  Identity t _ -> t
  If t _ _ _ -> t
  Intrinsic t _ _ -> t
  Lambda t _ _ _ _ -> t
  Match t _ _ _ -> t
  New t _ _ -> t
  NewClosure t _ _ -> t
  NewVector t _ _ -> t
  Push t _ _ -> t
  Swap t _ -> t




valueKinded :: [Type] -> [Type]
valueKinded = filter $ (Value ==) . typeKind




----------------------------------------
-- Unification
----------------------------------------




-- There are two kinds of unification going on here: basic logical unification
-- for value types, and row unification for effect types.

unifyType :: TypeEnv -> Type -> Type -> K TypeEnv
unifyType tenv0 t1 t2 = case (t1', t2') of
  _ | t1' == t2' -> return tenv0
  (TypeVar origin x, t) -> unifyTv tenv0 origin x t
  (_, TypeVar{}) -> commute
  -- FIXME: Unify the kinds here?
  (a, Forall origin (Var x k) t) -> do
    (b, _, tenv1) <- instantiate tenv0 origin x k t
    unifyType tenv1 a b
  (Forall{}, _) -> commute

  (TypeConstructor _ "join" :@ l :@ r, s) -> do
    ms <- rowIso tenv0 l s (effectTail r)
    case ms of
      Just (s', substitution, tenv1)
        -> case substitution of
          Just (x, t) -> let
            tenv2 = tenv1 { tenvTvs = Map.insert x t $ tenvTvs tenv1 }
            in unifyType tenv2 r s'
          Nothing -> unifyType tenv1 r s'

      Nothing -> do
        report $ Report Error
          [ Item (typeOrigin t1') ["I can't match the effect type", pQuote t1']
          , Item (typeOrigin t2') ["with the effect type", pQuote t2']
          ]
        halt

  (_, TypeConstructor _ "join" :@ _ :@ _) -> commute

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and effect type constructors.

  (a :@ b, c :@ d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d

  _ -> do
    report $ Report Error
      [ Item (typeOrigin t1') ["I can't match the type", pQuote t1']
      , Item (typeOrigin t2') ["with the type", pQuote t2']
      ]
    halt

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  t1' = zonkType tenv0 t1
  t2' = zonkType tenv0 t2
  commute = unifyType tenv0 t2 t1
  effectTail (TypeConstructor _ "join" :@ _ :@ a) = effectTail a
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

unifyTv :: TypeEnv -> Origin -> Var -> Type -> K TypeEnv
unifyTv tenv0 origin v@(Var x _) t = case t of
  TypeVar _origin (Var y _) | x == y -> return tenv0
  TypeVar{} -> declare
  _ -> if occurs tenv0 x t
    then let t' = zonkType tenv0 t in do
      report $ Report Error $
        [ Item origin
          ["I can't match the type", pQuote v, "with the type", pQuote t']
        , Item (typeOrigin t')
          ["because", pQuote v, "occurs in", pQuote t']
        ] ++ case t' of
          TypeConstructor _ "prod" :@ _ :@ _ -> [Item (typeOrigin t')
            ["possibly due to a stack depth mismatch"]]
          _ -> []
      halt
    else declare
  where
  declare = return tenv0 { tenvTvs = Map.insert x t $ tenvTvs tenv0 }

{-
  declare = case Map.lookup x $ tenvTvs tenv0 of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> return tenv0 { tenvTvs = Map.insert x t $ tenvTvs tenv0 }
-}




-- A convenience function for unifying a type with a function type.

unifyFunction :: TypeEnv -> Type -> K (Type, Type, Type, TypeEnv)
unifyFunction tenv0 t = case t of
  TypeConstructor _ "fun" :@ a :@ b :@ e -> return (a, b, e, tenv0)
  _ -> do
    let origin = typeOrigin t
    a <- freshTv tenv0 origin Stack
    b <- freshTv tenv0 origin Stack
    e <- freshTv tenv0 origin Effect
    tenv1 <- unifyType tenv0 t $ funType origin a b e
    return (a, b, e, tenv1)




-- Row unification is essentially unification of sets. The row-isomorphism
-- operation (as described in [1]) takes an effect label and an effect row, and
-- asserts that the row can be rewritten to begin with that label under some
-- substitution. It returns the substitution and the tail of the rewritten
-- row. The substitution is always either empty (∅) or a singleton substitution
-- (x ↦ τ).

rowIso
  :: TypeEnv -> Type -> Type -> Type
  -> K (Maybe (Type, Maybe (TypeId, Type), TypeEnv))

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 l (TypeConstructor _ "join" :@ l' :@ r') _
  | l == l' = return $ Just (r', Nothing :: Maybe (TypeId, Type), tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l (TypeConstructor origin "join" :@ l' :@ r') rt
  | l /= l' = do
  ms <- rowIso tenv0 l r' rt
  return $ case ms of
    Just (r'', substitution, tenv1) -> Just
      (joinType origin l' r'', substitution, tenv1)
    Nothing -> Nothing

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail. Here we enforce a side condition
-- that ensures termination by preventing unification of rows with a common tail
-- but distinct prefixes.

rowIso tenv0 l r@(TypeVar origin (Var a _)) rt
  | r /= rt = do
  b <- freshTv tenv0 origin Effect
  return $ Just (b, Just (a, joinType origin l b), tenv0)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ _ = return Nothing




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
  let
    forallAnonymous = Forall (signatureOrigin signature0)
    forallVar (var, origin) = Forall origin var
  return
    $ foldr forallAnonymous
      (foldr forallVar type_ $ Map.elems $ sigEnvVars env)
    $ sigEnvAnonymous env
  where

  go :: Signature -> StateT SignatureEnv K Type
  go signature = case signature of
    ApplicationSignature a b _ -> (:@) <$> go a <*> go b
    FunctionSignature as bs es origin -> do
      r <- lift $ freshTypeId tenv
      let var = Var r Stack
      let typeVar = TypeVar origin var
      es' <- mapM (fromVar origin) es
      (me, es'') <- lift $ effectVar origin es'
      Forall origin var <$> makeFunction origin typeVar as typeVar bs es'' me
    QuantifiedSignature vars a origin -> do
      original <- get
      (envVars, vars') <- foldrM ((lift .) . declare)
        (sigEnvVars original, []) vars
      modify $ \ env -> env { sigEnvVars = envVars }
      a' <- go a
      let result = foldr (Forall origin) a' vars'
      put original
      return result
      where

      declare
        :: (Unqualified, Kind, Origin)
        -> (Map Unqualified (Var, Origin), [Var])
        -> K (Map Unqualified (Var, Origin), [Var])
      declare (name, kind, varOrigin) (envVars, freshVars) = do
        x <- freshTypeId tenv
        let var = Var x kind
        return (Map.insert name (var, varOrigin) envVars, var : freshVars)

    SignatureVariable name origin -> fromVar origin name
    StackFunctionSignature r as s bs es origin -> do
      let var = fromVar origin
      r' <- var $ UnqualifiedName r
      s' <- var $ UnqualifiedName s
      es' <- mapM var es
      (me, es'') <- lift $ effectVar origin es'
      makeFunction origin r' as s' bs es'' me

  effectVar :: Origin -> [Type] -> K (Maybe Type, [Type])
  effectVar origin types = case findIndex isTypeVar types of
    Just index -> case splitAt index types of
      (preceding, type_ : following) -> case find isTypeVar following of
        Nothing -> return (Just type_, preceding ++ following)
        Just type' -> do
          report $ Report Error [Item origin
            [ "this signature has multiple effect variables:"
            , pQuote type_, "and", pQuote type'
            ]]
          halt
      _ -> error "splitting effect row"
    Nothing -> return (Nothing, types)
    where
    isTypeVar TypeVar{} = True
    isTypeVar _ = False

  fromVar :: Origin -> GeneralName -> StateT SignatureEnv K Type
  fromVar origin (UnqualifiedName name) = do
    existing <- gets $ Map.lookup name . sigEnvVars
    case existing of
      Just (var, varOrigin) -> return $ TypeVar varOrigin var
      Nothing -> lift $ do
        report $ Report Error [Item origin
          [ "I can't tell which type", pQuote name
          , "refers to. Did you mean to add it as a type parameter?"
          ]]
        halt
  fromVar origin (QualifiedName name)
    = return $ TypeConstructor origin $ Constructor name
  fromVar _ name = error
    $ "incorrectly resolved name in signature: " ++ show name

  makeFunction
    :: Origin
    -> Type -> [Signature] -> Type -> [Signature] -> [Type] -> Maybe Type
    -> StateT SignatureEnv K Type
  makeFunction origin r as s bs es me = do
    as' <- mapM go as
    bs' <- mapM go bs
    e <- case me of
      Just e -> return e
      Nothing -> do
        ex <- lift $ freshTypeId tenv
        let var = Var ex Effect
        modify $ \ env -> env { sigEnvAnonymous = var : sigEnvAnonymous env }
        return $ TypeVar origin var
    return $ funType origin (stack r as') (stack s bs')
      $ foldr (joinType origin) e es
    where

    stack :: Type -> [Type] -> Type
    stack = foldl' $ prodType origin

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: [Var]
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
    TypeVar _origin (Var x _k) -> case Map.lookup x (tenvTvs tenv0) of
      -- FIXME: Is this necessary?
      -- Just (TypeVar _origin (Var x' _)) | x == x' -> TypeVar origin (Var x k)
      Just t' -> recur t'
      Nothing -> t
    TypeConstant{} -> t
    Forall origin (Var x k) t' -> Forall origin (Var x k)
      $ zonkType tenv0 { tenvTvs = Map.delete x (tenvTvs tenv0) } t'
    a :@ b -> recur a :@ recur b




-- Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.

zonkTerm :: TypeEnv -> Term -> Term
zonkTerm tenv0 = go
  where
  go term = case term of
    Call tref fixity name params origin
      -> Call (zonkMay tref) fixity name params origin
    Compose tref a b
      -> Compose (zonkMay tref) (go a) (go b)
    Drop tref origin
      -> Drop (zonkMay tref) origin
    Generic x a origin
      -> Generic x (go a) origin
    Group a
      -> go a
    Identity tref origin
      -> Identity (zonkMay tref) origin
    If tref true false origin
      -> If (zonkMay tref) (go true) (go false) origin
    Intrinsic tref name origin
      -> Intrinsic (zonkMay tref) name origin
    Lambda tref name varType body origin
      -> Lambda (zonkMay tref) name (zonkMay varType) (go body) origin
    Match tref cases mElse origin
      -> Match (zonkMay tref) (map goCase cases) (fmap goElse mElse) origin
      where
      goCase (Case name body caseOrigin)
        = Case name (go body) caseOrigin
      goElse (Else body elseOrigin)
        = Else (go body) elseOrigin
    New tref index origin
      -> New (zonkMay tref) index origin
    NewClosure tref index origin
      -> NewClosure (zonkMay tref) index origin
    NewVector tref size origin
      -> NewVector (zonkMay tref) size origin
    Push tref value origin
      -> Push (zonkMay tref) (zonkValue tenv0 value) origin
    Swap tref origin
      -> Swap (zonkMay tref) origin
  zonkMay = fmap $ zonkType tenv0

zonkValue :: TypeEnv -> Value -> Value
zonkValue tenv0 = go
  where
  go value = case value of
    Boolean{} -> value
    Character{} -> value
    Closed{} -> value
    Closure names body -> Closure names $ zonkTerm tenv0 body
    Float{} -> value
    Integer{} -> value
    Local{} -> value
    Name{} -> value
    Quotation body -> Quotation $ zonkTerm tenv0 body
    Text{} -> value




--------------------------------------------------------------------------------
-- Instantiation and Regeneralization
--------------------------------------------------------------------------------




-- To instantiate a type scheme, we simply replace all quantified variables with
-- fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order. Because type identifiers are globally
-- unique, we know a fresh type variable will never be erroneously captured.

instantiate
  :: TypeEnv -> Origin -> TypeId -> Kind -> Type -> K (Type, Type, TypeEnv)
instantiate tenv0 origin x k t = do
  ia <- freshTypeId tenv0
  let a = TypeVar origin $ Var ia k
  replaced <- replaceTv tenv0 x a t
  return (replaced, a, tenv0)




-- When generating an instantiation of a generic definition, we only want to
-- instantiate the rank-1 quantifiers; all other quantifiers are irrelevant.

instantiatePrenex :: TypeEnv -> Type -> K (Type, [Type], TypeEnv)
instantiatePrenex tenv0 _q@(Forall origin (Var x k) t)
  = {- while ["instantiating", show q] $ -} do
    (t', a, tenv1) <- instantiate tenv0 origin x k t
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
  in foldr (uncurry ((Forall (typeOrigin t) .) . Var)) t'
    $ foldr (deleteBy ((==) `on` fst)) (Map.toList (freeTvks t')) vars
  where
  go :: Type -> Writer [(TypeId, Kind)] Type
  go t' = case t' of
    TypeConstructor _ "fun" :@ a :@ b :@ e
      | TypeVar origin (Var c k) <- bottommost a
      , TypeVar _ (Var d _) <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [(c, k)]
        a' <- go a
        b' <- go b
        e' <- go e
        return $ Forall origin (Var c k) $ funType origin a' b' e'
    c@(TypeConstructor _ "prod") :@ a :@ b -> do
      a' <- go a
      b' <- go b
      return $ c :@ a' :@ b'
    -- FIXME: This should descend into the quantified type.
    Forall{} -> return t'
    a :@ b -> (:@) <$> go a <*> go b
    _ -> return t'




bottommost :: Type -> Type
bottommost (TypeConstructor _ "prod" :@ a :@ _) = bottommost a
bottommost a = a




----------------------------------------
-- Instance Checking
----------------------------------------




-- Since skolem constants only unify with type variables and themselves,
-- unifying a skolemized scheme with a type tells you whether one is a generic
-- instance of the other. This is used to check the signatures of definitions.

instanceCheck :: Pretty.Doc -> Type -> Pretty.Doc -> Type -> K ()
instanceCheck aSort aScheme bSort bScheme = do
  let tenv0 = emptyTypeEnv
  (aType, _, tenv1) <- instantiatePrenex tenv0 aScheme
  (ids, bType) <- skolemize tenv1 bScheme
  check <- K $ \ env -> do
    result <- runK (subsumptionCheck tenv1 aType bType) env
    return $ Just result
  case check of
    Nothing -> failure
    _ -> return ()
  let escaped = freeTvs aScheme `Set.union` freeTvs bScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) failure
  return ()
  where
  failure = report $ Report Error
    [ Item (typeOrigin aScheme)
      ["I couldn't match this", aSort, "type", pQuote aScheme]
    , Item (typeOrigin bScheme)
      [ "with the", bSort, "type signature"
      , pQuote bScheme
      ]
    ]




-- Skolemization replaces quantified type variables with type constants.

skolemize :: TypeEnv -> Type -> K (Set TypeId, Type)
skolemize tenv0 t = case t of
  Forall origin (Var x k) t' -> do
    c <- freshTypeId tenv0
    let tenv1 = tenv0 { tenvTvs = Map.insert x (TypeConstant origin $ Var c k)
        $ tenvTvs tenv0 }
    (c', t'') <- skolemize tenv1 $ zonkType tenv1 t'
    return (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  TypeConstructor origin "fun" :@ a :@ b :@ e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, funType origin a b' e)
  _ -> return (Set.empty, t)




-- Subsumption checking is largely the same as unification, except for the fact
-- that a function type is contravariant in its input type.

subsumptionCheck :: TypeEnv -> Type -> Type -> K TypeEnv
subsumptionCheck tenv0 (Forall origin (Var x k) t) t2 = do
  (t1, _, tenv1) <- instantiate tenv0 origin x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 (TypeConstructor _ "fun" :@ a' :@ b' :@ e') = do
  (a, b, e, tenv1) <- unifyFunction tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 (TypeConstructor _ "fun" :@ a :@ b :@ e) t2 = do
  (a', b', e', tenv1) <- unifyFunction tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 t1 t2 = unifyType tenv0 t1 t2

subsumptionCheckFun
  :: TypeEnv -> Type -> Type -> Type -> Type -> Type -> Type -> K TypeEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  let
    labels = effectList $ zonkType tenv2 e
    labels' = effectList $ zonkType tenv2 e'
  forM_ labels $ \ (origin, label) -> case find ((label ==) . snd) labels' of
    Just{} -> return ()
    Nothing -> report $ Report Error
      [ Item (typeOrigin e)
        ["I can't match the effect type", pQuote e]
      , Item (typeOrigin e')
        $ ["with the effect type", pQuote e']
      , Item origin
        ["because the effect label", pQuote label, "is missing"]
      ]
  return tenv2
  where

  effectList :: Type -> [(Origin, Constructor)]
  effectList (TypeConstructor _ "join" :@ TypeConstructor origin label :@ es)
    = (origin, label) : effectList es
  effectList _ = []




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

collectInstantiations :: TypeEnv -> Program -> K Program
collectInstantiations tenv0 program0 = do

-- We enqueue all the instantiation sites reachable from the top level of the
-- program, and any non-generic definitions.

  (definitions, q0) <- foldrM
    (\ ((name, type_), expr) (acc, q) -> do
      (expr', q') <- go q expr
      return (((name, type_), expr') : acc, q'))
    ([], emptyQueue)
    $ HashMap.toList $ programDefinitions program0

-- Next, we process the queue. Doing so may enqueue new instantiation sites for
-- processing; however, this is guaranteed to halt because the number of actual
-- instantiations is finite.

  definitions' <- processQueue q0 $ HashMap.fromList definitions
  return program0 { programDefinitions = definitions' }

  where

-- We process a definition in a single pass, mangling all its call sites and
-- enqueueing them for instantiation. We perform the actual instantiation while
-- processing the queue, so as to avoid redundant instantiations.

  go :: InstantiationQueue -> Term -> K (Term, InstantiationQueue)
  go q0 term = case term of
    Call type_ fixity (QualifiedName name) args origin -> return
      ( Call type_ fixity
        (UnqualifiedName (Unqualified (mangleName name args))) [] origin
      , enqueue (name, args) q0
      )
    -- FIXME: Should calls to non-qualified names even be around at this point?
    Call{} -> proceed
    Compose type_ a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      return (Compose type_ a' b', q2)
    Drop{} -> proceed

-- If the definition is generic, we simply ignore it; we won't find any
-- instantiations in it, because it's not instantiated itself!

    Generic{} -> proceed
    Group{} -> error "group should not appear after linearization"
    Identity{} -> proceed
    If type_ true false origin -> do
      (true', q1) <- go q0 true
      (false', q2) <- go q1 false
      return (If type_ true' false' origin, q2)
    Intrinsic{} -> proceed
    Lambda type_ name varType body origin -> do
      (body', q1) <- go q0 body
      return (Lambda type_ name varType body' origin, q1)
    Match type_ cases mElse origin -> do
      (cases', q1) <- foldrM (\ (Case name body caseOrigin) (bodies, q) -> do
        (body', q') <- go q body
        return (Case name body' caseOrigin : bodies, q'))
        ([], q0) cases
      (mElse', q2) <- case mElse of
        Just (Else body elseOrigin) -> do
          (body', q') <- go q1 body
          return (Just (Else body' elseOrigin), q')
        Nothing -> return (Nothing, q1)
      return (Match type_ cases' mElse' origin, q2)
    New{} -> proceed
    NewClosure{} -> proceed
    NewVector{} -> proceed
    Push _ Quotation{} _ -> error
      "quotation should not appear after quotation desugaring"
    Push{} -> proceed
    Swap{} -> proceed
    where
    proceed = return (term, q0)

-- Processing the queue operates by dequeueing and instantiating each definition
-- in turn. If the definition has already been instantiated, we simply proceed.

  processQueue
    :: InstantiationQueue
    -> HashMap (Qualified, Type) Term
    -> K (HashMap (Qualified, Type) Term)
  processQueue q defs = case dequeue q of
    Nothing -> return defs
    Just ((name, args), q') -> let
      mangled = mangleName name args
      name' = Qualified globalVocabulary $ Unqualified mangled
      in case lookupWith ((name' ==) . fst) defs of
        Just{} -> processQueue q' defs
        Nothing -> case lookupWith ((name ==) . fst) defs of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' defs
          Just ((_, type_), term) -> do
            term' <- instantiateExpr tenv0 term args
            (term'', q'') <- go q' term'
            processQueue q'' $ HashMap.insert
              (Qualified globalVocabulary (Unqualified mangled), type_)
              term'' defs

type InstantiationQueue = Queue (Qualified, [Type])

-- FIXME: This should be made more efficient.
lookupWith :: (k -> Bool) -> HashMap k v -> Maybe (k, v)
lookupWith f = find (f . fst) . HashMap.toList




-- Names are mangled according to the local C++ mangling convention. This is a
-- silly approximation of the IA-64 convention for testing purposes.

mangleName :: Qualified -> [Type] -> Text
mangleName name args = mconcat . (["_Z", lengthEncode name] ++)
  $ if null args then [] else ["I", Text.concat $ map mangleType args, "E"]

mangleType :: Type -> Text
mangleType (TypeConstructor _ "fun" :@ _ :@ _) = "PFvv"
mangleType (TypeConstructor _ "ptr" :@ a) = Text.concat ["P", mangleType a]
mangleType (TypeConstructor _origin (Constructor con)) = case con of
  Qualified qualifier name | qualifier == globalVocabulary -> case name of
    "int" -> "i"
    "bool" -> "b"
    "char" -> "c"
    "float" -> "f"
    "text" -> "PKc"
    _ -> lengthEncode con
  _ -> lengthEncode con
mangleType (TypeConstructor _ "prod" :@ a :@ b)
  = Text.concat $ map mangleType [a, b]
mangleType t = Text.concat ["(", Text.pack (Pretty.render (pPrint t)), ")"]

lengthEncode :: Qualified -> Text
lengthEncode (Qualified (Qualifier parts) (Unqualified name)) = case parts of
  [] -> lengthEncode' name
  [""] -> lengthEncode' name
  _ -> mconcat $ "N" : map lengthEncode' (parts ++ [name]) ++ ["E"]
  where
  lengthEncode' part = mconcat [Text.pack $ show $ Text.length part, part]




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
instantiateExpr tenv term args = foldlM go term args
  where
  go (Generic x expr _origin) arg = replaceTvExpr tenv x arg expr
  go _ _ = do
    report $ Report Error $ Item (termOrigin term)
      [ "wrong number of type arguments instantiating"
      , pPrint term Pretty.<> ":"
      ] : map (\ arg -> Item (typeOrigin arg) [pPrint (zonkType tenv arg)]) args
    halt




-- Copies the top-level generic value-kinded type quantifiers from a polytype to
-- an expression, thereby making the expression generic, e.g.:
--
--     ∀α:ρ. ∀β:*. ∀γ:Ε. (α × β → α × β × β) Ε    dup
--
--     Λβ:*. dup

quantifyTerm :: Type -> Term -> Term
quantifyTerm (Forall origin (Var x Value) t) e
  = Generic x (quantifyTerm t e) origin
quantifyTerm (Forall _ _ t) e = quantifyTerm t e
quantifyTerm _ e = e

quantifyTerms :: Program -> Program
quantifyTerms program = program
  { programDefinitions = HashMap.mapWithKey
    (\ (_, type_) term -> quantifyTerm type_ term)
    $ programDefinitions program }




----------------------------------------
-- Substitution
----------------------------------------




-- Capture-avoiding substitution of a type variable α with a type τ throughout a
-- type σ, [α ↦ τ]σ.

replaceTv :: TypeEnv -> TypeId -> Type -> Type -> K Type
replaceTv tenv0 x a = recur
  where
  recur t = case t of
    Forall origin (Var x' k) t'
      | x == x' -> return t
      | x' `Set.notMember` freeTvs t' -> Forall origin (Var x' k) <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- replaceTv tenv0 x' (TypeVar origin $ Var z k) t'
        Forall origin (Var z k) <$> recur t''
    TypeVar _ (Var x' _) | x == x' -> return a
    m :@ n -> (:@) <$> recur m <*> recur n
    _ -> return t




replaceTvExpr :: TypeEnv -> TypeId -> Type -> Term -> K Term
replaceTvExpr tenv x a = recur
  where
  recur term = case term of
    Call tref fixity name args origin -> Call <$> go' tref
      <*> pure fixity <*> pure name <*> mapM go args <*> pure origin
    Compose tref t1 t2 -> Compose <$> go' tref <*> recur t1 <*> recur t2
    Drop tref origin -> Drop <$> go' tref <*> pure origin
    Generic x' _ _ -> if x == x' then return term
      else error
        "substituting in a generic term should not require capture-avoidance"
    Group t -> recur t
    Identity tref origin -> Identity <$> go' tref <*> pure origin
    If tref true false origin -> If <$> go' tref
      <*> recur true <*> recur false <*> pure origin
    Intrinsic tref name origin -> Intrinsic <$> go' tref
      <*> pure name <*> pure origin
    Lambda tref name varType body origin -> Lambda <$> go' tref
      <*> pure name <*> go' varType <*> recur body <*> pure origin
    Match tref cases mElse origin -> Match <$> go' tref
      <*> mapM goCase cases <*> traverse goElse mElse <*> pure origin
      where

      goCase :: Case -> K Case
      goCase (Case name body caseOrigin)
        = Case name <$> recur body <*> pure caseOrigin

      goElse :: Else -> K Else
      goElse (Else body elseOrigin) = Else <$> recur body <*> pure elseOrigin

    New tref index origin -> New <$> go' tref <*> pure index <*> pure origin
    NewClosure tref size origin -> NewClosure <$> go' tref
      <*> pure size <*> pure origin
    NewVector tref size origin -> NewVector <$> go' tref
      <*> pure size <*> pure origin
    Push tref value origin -> Push <$> go' tref <*> pure value <*> pure origin
    Swap tref origin -> Swap <$> go' tref <*> pure origin

  go t = replaceTv tenv x a t
  go' (Just t) = Just <$> go t
  go' Nothing = return Nothing




----------------------------------------
-- Free Variables
----------------------------------------




-- The free variables of a type are those not bound by any quantifier.

freeTvs :: Type -> Set TypeId
freeTvs = Set.fromList . Map.keys . freeTvks

freeTvks :: Type -> Map TypeId Kind
freeTvks t = case t of
  TypeConstructor{} -> Map.empty
  TypeVar _ (Var x k) -> Map.singleton x k
  TypeConstant{} -> Map.empty
  Forall _ (Var x _) t' -> Map.delete x $ freeTvks t'
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
    TypeVar _ (Var y _) -> case Map.lookup y (tenvTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TypeConstant{} -> 0
    Forall _ (Var x' _) t' -> if x == x' then 0 else recur t'
    a :@ b -> recur a + recur b

occurs :: TypeEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0




--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------




instance Pretty Token where
  pPrint token = case token of
    AngleToken{} -> ">"
    ArrowToken{} -> "->"
    BlockBeginToken _ _ _ -> "{"
    BlockEndToken{} -> "}"
    BooleanToken True _ _ -> "true"
    BooleanToken False _ _ -> "false"
    CaseToken{} -> "case"
    CharacterToken c _ _ -> Pretty.quotes $ Pretty.char c
    CommaToken{} -> ","
    DataToken{} -> "data"
    DefineToken{} -> "define"
    EllipsisToken{} -> "..."
    ElifToken{} -> "elif"
    ElseToken{} -> "else"
    FloatToken f _ _ -> Pretty.double f
    GroupBeginToken{} -> "("
    GroupEndToken{} -> ")"
    IfToken{} -> "if"
    IgnoreToken{} -> "_"
    InfixToken{} -> "infix"
    InstanceToken{} -> "instance"
    IntegerToken value hint _ _
      -> Pretty.text $ if value < 0 then '-' : shown else shown
      where
      shown = prefix ++ showIntAtBase base (digits !!) (abs value) ""
      (base, prefix, digits) = case hint of
        Binary -> (2, "0b", "01")
        Octal -> (8, "0o", ['0'..'7'])
        Decimal -> (10, "", ['0'..'9'])
        Hexadecimal -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
    LayoutToken{} -> ":"
    MatchToken{} -> "match"
    OperatorToken name _ _ -> pPrint name
    ReferenceToken{} -> "\\"
    SynonymToken{} -> "synonym"
    TextToken t _ _ -> Pretty.doubleQuotes $ Pretty.text $ Text.unpack t
    TraitToken{} -> "trait"
    VectorBeginToken{} -> "["
    VectorEndToken{} -> "]"
    VocabToken{} -> "vocab"
    VocabLookupToken{} -> "::"
    WordToken name _ _ -> pPrint name

instance Pretty Fragment where
  pPrint fragment = prettyVsep $ concat docs
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

instance Pretty Program where
  pPrint Program { programDefinitions = definitions } = prettyVsep
    $ map (\ ((name, type_), term) -> pPrintAsDefinition
      (pPrint name) (pPrint type_) (pPrint term)
      (pPrint (DefineToken Anywhere Nothing)))
    $ HashMap.toList definitions

prettyVsep :: [Pretty.Doc] -> Pretty.Doc
prettyVsep = Pretty.vcat . intersperse ""

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
  pPrint definition = pPrintAsDefinition
    (pPrint $ definitionName definition)
    (pPrint $ definitionSignature definition)
    (pPrint $ definitionBody definition)
    $ pPrint $ DefineToken Anywhere Nothing

pPrintAsDefinition
  :: Pretty.Doc -> Pretty.Doc -> Pretty.Doc-> Pretty.Doc -> Pretty.Doc
pPrintAsDefinition name signature body keyword = Pretty.vcat
  [ Pretty.hcat
    [Pretty.hsep [keyword, name, signature], ":"]
  , Pretty.nest 4 body
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
    Call _ _ name [] _ -> pPrint name
    Call _ _ name args _ -> Pretty.hcat
      $ pPrint name : "<" : intersperse ", " (map pPrint args) ++ [">"]
    Compose _ a b -> pPrint a Pretty.$+$ pPrint b
    Drop _ _ -> "drop"
    Generic name body _ -> Pretty.hsep
      [prettyAngles $ pPrint name, pPrint body]
    Group a -> Pretty.parens (pPrint a)
    Identity{} -> Pretty.empty
    If _ a b _ -> "if:"
      Pretty.$$ Pretty.nest 4 (pPrint a)
      Pretty.$$ "else:"
      Pretty.$$ Pretty.nest 4 (pPrint b)
    Intrinsic _ name _ -> pPrint name
    Lambda _ name _ body _ -> "->"
      Pretty.<+> pPrint name
      Pretty.<> ";"
      Pretty.$+$ pPrint body
    Match _ cases mElse _ -> Pretty.vcat
      [ "match:"
      , Pretty.nest 4 $ Pretty.vcat $ map pPrint cases
        ++ [pPrint else_ | Just else_ <- [mElse]]
      ]
    New _ index _ -> "new." Pretty.<> pPrint index
    NewClosure _ size _ -> "new.closure." Pretty.<> pPrint size
    NewVector _ size _ -> "new.vec." Pretty.<> pPrint size
    Push _ value _ -> pPrint value
    Swap{} -> "swap"

instance Pretty Case where
  pPrint (Case name body _) = Pretty.vcat
    [ Pretty.hcat ["case ", pPrint name, ":"]
    , Pretty.nest 4 $ pPrint body
    ]

instance Pretty Else where
  pPrint (Else body _) = Pretty.vcat ["else:", Pretty.nest 4 $ pPrint body]

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
    Name n -> pPrint n
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
  pPrint intrinsic = case intrinsic of
    AddIntrinsic -> ".add.int"
    MagicIntrinsic -> ".magic"

-- FIXME: Real instance.
instance Pretty Synonym where
  pPrint _ = "synonym"

instance Pretty Signature where
  pPrint signature = case signature of
    ApplicationSignature a b _ -> Pretty.hcat
      [pPrint a, prettyAngles $ pPrint b]
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
    TypeConstructor _ "fun" :@ a :@ b :@ e -> Pretty.parens
      $ Pretty.hsep [pPrint a, "->", pPrint b, pPrint e]
    TypeConstructor _ "prod" :@ a :@ b
      -> Pretty.hcat [pPrint a, ", ", pPrint b]
    TypeConstructor _ "join" :@ a :@ b
      -> Pretty.hcat ["+", pPrint a, " ", pPrint b]
    a :@ b -> Pretty.hcat [pPrint a, prettyAngles $ pPrint b]
    TypeConstructor _ constructor -> pPrint constructor
    TypeVar _ var -> pPrint var
    TypeConstant _ var -> pPrint var
    Forall{} -> prettyForall type_ []
      where
      prettyForall (Forall _ x t) vars = prettyForall t (x : vars)
      prettyForall t vars = Pretty.hcat
        [ prettyAngles $ prettyList $ map pPrint vars
        , Pretty.parens $ pPrint t
        ]

instance Pretty Constructor where
  pPrint (Constructor name) = pPrint name

instance Pretty Kind where
  pPrint kind = case kind of
    Value -> "value"
    Stack -> "stack"
    Label -> "label"
    Effect -> "effect"
    a :-> b -> Pretty.parens $ Pretty.hsep
      [pPrint a, "->", pPrint b]

instance Pretty Var where
  pPrint (Var i kind) = Pretty.hcat $ case kind of
    Effect -> ["+", v]
    Stack -> [v, "..."]
    _ -> [v]
    where
    v = pPrint i

instance Pretty TypeId where
  pPrint (TypeId i) = Pretty.hcat [Pretty.char 'T', Pretty.int i]

prettyAngles :: Pretty.Doc -> Pretty.Doc
prettyAngles doc = Pretty.hcat [Pretty.char '<', doc, Pretty.char '>']

prettyList :: [Pretty.Doc] -> Pretty.Doc
prettyList = Pretty.hcat . intersperse ", "

pQuote :: (Pretty a) => a -> Pretty.Doc
pQuote = Pretty.quotes . pPrint

oxfordOr :: [Pretty.Doc] -> Pretty.Doc
oxfordOr [] = ""
oxfordOr [x] = x
oxfordOr [x, y] = Pretty.hcat [x, "or", y]
oxfordOr [x, y, z] = Pretty.hcat [x, ", ", y, ", or ", z]
oxfordOr (x : xs) = Pretty.hcat [x, ", ", oxfordOr xs]





--------------------------------------------------------------------------------
-- K Monad
--------------------------------------------------------------------------------




newtype K a = K { runK :: Env -> IO (Maybe a) }

data Env = Env { envContext :: [Report], envReports :: IORef [[Report]] }

data Report = Report !Category [Item]
  deriving (Eq)

data Item = Item !Origin [Pretty.Doc]
  deriving (Eq)

data Category = Note | Warning | Error
  deriving (Eq)

instance Functor K where
  fmap f (K ax) = K $ fmap (fmap f) . ax

instance Applicative K where
  pure = K . const . return . Just
  K af <*> K ax = K $ \ env -> do
    mf <- af env
    mx <- ax env
    return $ case (mf, mx) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just f, Just x) -> Just (f x)

instance Monad K where
  return = K . const . return . Just
  K ax >>= f = K $ \ env -> do
    mx <- ax env
    case mx of
      Nothing -> return Nothing
      Just x -> runK (f x) env
  fail = error "do not use 'fail'"

instance MonadFix K where
  mfix k = K $ \ env -> do
    m <- newEmptyMVar
    a <- unsafeInterleaveIO $ takeMVar m
    mx <- runK (k a) env
    case mx of
      Nothing -> return Nothing
      Just x -> do
        putMVar m x
        return mx

instance MonadIO K where
  liftIO = K . const . fmap Just

checkpoint :: K ()
checkpoint = K $ \ env -> do
  errors <- readIORef (envReports env)
  return (if null errors then Just () else Nothing)

report :: Report -> K ()
report r = K $ \ env -> Just
  <$> modifyIORef (envReports env) ((r : envContext env) :)

halt :: K a
halt = K $ const $ return Nothing

while :: Report -> K a -> K a
while prefix action = K $ \ env
  -> runK action env { envContext = prefix : envContext env }

reportParseError :: Parsec.ParseError -> Report
reportParseError parseError = Report Error
  $ unexpecteds ++ if null expected then [] else [Item origin
    [ "expecting"
    , oxfordOr $ map (Pretty.quotes . Pretty.text) $ nub $ filter (not . null)
      $ map Parsec.messageString expected
    ]]
  where

  origin :: Origin
  origin = Range
    { rangeBegin = Parsec.errorPos parseError
    , rangeEnd = Parsec.errorPos parseError
    }

  sysUnexpected, unexpected, expected :: [Parsec.Message]
  (sysUnexpected, unexpected, expected)
    = flip evalState (Parsec.errorMessages parseError) $ (,,)
      <$> state (span (Parsec.SysUnExpect "" ==))
      <*> state (span (Parsec.UnExpect "" ==))
      <*> state (span (Parsec.Expect "" ==))

  unexpecteds :: [Item]
  unexpecteds = ((++) `on` unexpectedMessages) sysUnexpected unexpected

  unexpectedMessages :: [Parsec.Message] -> [Item]
  unexpectedMessages = nub . map unexpectedMessage

  unexpectedMessage :: Parsec.Message -> Item
  unexpectedMessage message = let
    string = Parsec.messageString message
    in Item origin
      [ "unexpected"
      , if null string then "end of input"
        else Pretty.quotes $ Pretty.text string
      ]

showReport :: Report -> String
showReport (Report category messages) = unlines $ map showItem $ prefix messages
  where
  prefix (Item origin firstItem : rest)
    = Item origin (categoryPrefix category : firstItem) : rest
  prefix [] = []
  showItem (Item origin message)
    = showOriginPrefix origin ++ Pretty.render (Pretty.hsep message)

showOriginPrefix :: Origin -> String
showOriginPrefix (Range a b) = concat
  $ [Parsec.sourceName a, ":", show al, ".", show ac, "-"]
  ++ (if al == bl then [show bc] else [show bl, ".", show bc])
  ++ [": "]
  where
  al = Parsec.sourceLine a
  bl = Parsec.sourceLine b
  ac = Parsec.sourceColumn a
  bc = Parsec.sourceColumn b
showOriginPrefix _ = "<unknown location>: "

categoryPrefix :: Category -> Pretty.Doc
categoryPrefix category = case category of
  Note -> "note: "
  Warning -> "warning: "
  Error -> "error: "
