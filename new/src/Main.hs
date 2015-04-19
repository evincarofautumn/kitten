{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Char
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import GHC.Exts
import Numeric
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import Text.Parsec hiding ((<|>), letter, many, newline, optional, parse, token, tokens)
import Text.Parsec.Text ()
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expression

main :: IO ()
main = do
  liftIO $ hSetEncoding stdout utf8
  errors <- newIORef []
  paths <- liftIO getArgs
  result <- runK (compile paths) Env { envErrors = errors }
  case result of
    Nothing -> readIORef errors >>= mapM (hPutStrLn stderr) >> exitFailure
    Just fragment -> print fragment

compile :: [FilePath] -> K Fragment
compile paths = do
  sources <- liftIO (mapM readFileUtf8 paths)
  tokenized <- zipWithM tokenize paths sources
  checkpoint
  laidout <- zipWithM layout paths tokenized
  checkpoint
  parsed <- mconcat <$> zipWithM parse paths laidout
  checkpoint
  let substituted = substituteSynonyms parsed
  postfix <- desugarInfix substituted
  return postfix

substituteSynonyms :: Fragment -> Fragment
substituteSynonyms fragment = fragment {
  fragmentDefinitions = foldr
    (map . inDefinition)
    (fragmentDefinitions fragment)
    (fragmentSynonyms fragment) }
  where
  inDefinition :: Synonym -> Definition -> Definition
  inDefinition (Synonym from to _) definition
    = if nameVocabulary from == nameVocabulary (definitionName definition)
      then definition { definitionBody = inTerm (definitionBody definition) }
      else definition
    where
    inTerm :: Term -> Term
    inTerm term = case term of
      Call a b name origin
        | name == nameRelative from -> Call a b to origin
        | otherwise -> term
      Compose a b c -> Compose a (inTerm b) (inTerm c)
      Generic a b origin -> Generic a (inTerm b) origin
      If a b c origin -> If a (inTerm b) (inTerm c) origin
      Match a b c origin -> Match a (map inCase b) (fmap inElse c) origin
      Push a b origin -> Push a (inValue b) origin
      _ -> term
    inValue :: Value -> Value
    inValue value = case value of
      Quotation term -> Quotation (inTerm term)
      _ -> value
    inCase :: Case -> Case
    inCase (Case a b origin) = Case a (inTerm b) origin
    inElse :: Else -> Else
    inElse (Else a origin) = Else (inTerm a) origin

desugarInfix :: Fragment -> K Fragment
desugarInfix fragment = do
  desugared <- mapM desugarDefinition (fragmentDefinitions fragment)
  return fragment { fragmentDefinitions = desugared }
  where

  desugarDefinition :: Definition -> K Definition
  desugarDefinition definition = do
    desugared <- desugarTerm (definitionBody definition)
    return definition { definitionBody = desugared }

  desugarTerm :: Term -> K Term
  desugarTerm term = case term of
    Compose _ a b -> do
      a' <- desugarTerm a
      b' <- desugarTerm b
      let
        expression' = infixExpression <* eof
        infixExpression = compose (termOrigin a)  -- FIXME
          <$> many (expression {- <|> lambdaTerm -})
      case Parsec.runParser expression' () "" (Compose Nothing a' b') of
        Left parseError -> fail (show parseError)
        Right result -> return result
    _ -> return term

  expression :: Rewriter Term
  expression = Expression.buildExpressionParser operatorTable (operands <?> "operand")
    where
    operands = do
      origin <- getOrigin
      results <- operand
      return $ compose origin results
    operand = many1 $ termSatisfy $ \term -> case term of
      Call _ Infix _ _ -> False  -- An operator is not an operand.
      -- FIXME: What should be done about "lambdas"?
      _ -> True

  operatorTable :: [[Expression.Operator Term () Identity Term]]
  operatorTable = map (map toOperator) rawOperatorTable

  fragmentFixities :: Map Name Fixity
  fragmentFixities = Map.fromList (map (definitionName &&& definitionFixity) (fragmentDefinitions fragment))

  rawOperatorTable :: [[Operator]]
  rawOperatorTable = let
    useDefault name fixity
      = fixity == Infix
      && not (any ((name ==) . operatorName) (fragmentOperators fragment))
    flat = fragmentOperators fragment ++ map (\ name -> Operator {
      operatorAssociativity = Leftward,
      operatorName = name,
      operatorPrecedence = 6 })
      (Map.keys (Map.filterWithKey useDefault fragmentFixities))
    in map (\ p -> filter ((p ==) . operatorPrecedence) flat) [9, 8 .. 0]

  toOperator :: Operator -> Expression.Operator Term () Identity Term
  toOperator operator = Expression.Infix (binaryOperator (operatorName operator))
    $ case operatorAssociativity operator of
      Nonassociative -> Expression.AssocNone
      Leftward -> Expression.AssocRight
      Rightward -> Expression.AssocLeft

  binaryOperator :: Name -> Rewriter (Term -> Term -> Term)
  binaryOperator name = mapTerm $ \ term -> case term of
    Call _ Infix name' loc | name == name' -> Just (binary name loc)
    _ -> Nothing

  binary :: Name -> Origin -> Term -> Term -> Term
  binary name origin x y = compose origin [x, y, Call Nothing Postfix name origin]

  mapTerm :: (Term -> Maybe a) -> Rewriter a
  mapTerm = tokenPrim show advanceTerm

  termSatisfy :: (Term -> Bool) -> Rewriter Term
  termSatisfy predicate = tokenPrim show advanceTerm
    $ \ token -> if predicate token then Just token else Nothing

  advanceTerm :: SourcePos -> t -> Term -> SourcePos
  advanceTerm sourcePos _ stream = case runIdentity (uncons stream) of
    Just (next, _) -> rangeBegin (termOrigin next)
    Nothing -> sourcePos

instance (Monad m) => Stream Term m Term where
  uncons term = case term of
    Compose _ Compose{} _ -> error ("composition must be left-associative: " ++ show term)
    Compose _ a b -> return (Just (a, b))
    Identity{} -> return Nothing
    _ -> return (Just (term, Identity Nothing (termOrigin term)))

tokenize :: FilePath -> Text -> K [Token]
tokenize path text = case Parsec.runParser fileTokenizer 1 path text of
  Left parseError -> fail (show parseError)
  Right result -> return result

type Tokenizer a = ParsecT Text Column Identity a
type Parser a = ParsecT [Token] Name Identity a
type Rewriter a = ParsecT Term () Identity a

fileTokenizer :: Tokenizer [Token]
fileTokenizer = silenceTokenizer *> tokensTokenizer <* eof

silenceTokenizer :: Tokenizer ()
silenceTokenizer = skipMany $ comment <|> whitespace
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

tokensTokenizer :: Tokenizer [Token]
tokensTokenizer = tokenTokenizer `sepEndBy` silenceTokenizer

rangedTokenizer :: Tokenizer (Origin -> Indent -> Token) -> Tokenizer Token
rangedTokenizer parser = do
  column <- getState
  begin <- getPosition
  result <- parser
  end <- getPosition
  return (result (Range begin end) (Just column))

tokenTokenizer :: Tokenizer Token
tokenTokenizer = rangedTokenizer $ choice [
  BlockBeginToken Nonlayout <$ char '{',
  BlockEndToken <$ char '}',
  do
    mc <- char '\'' *> character '\'' <* char '\''
    case mc of
      Just c -> return (CharacterToken c)
      Nothing -> unexpected "empty character literal",
  CommaToken <$ char ',',
  try $ EllipsisToken <$ string "...",
  GroupBeginToken <$ char '(',
  GroupEndToken <$ char ')',
  try $ IgnoreToken <$ char '_' <* notFollowedBy letter,
  try $ VocabLookupToken <$ string "::",
  LayoutToken <$ char ':',
  VectorBeginToken <$ char '[',
  VectorEndToken <$ char ']',
  ReferenceToken <$ char '\\',
  TextToken <$> between (char '"') (char '"') text,
  try $ do
    sign <- optionMaybe $ oneOf "+-"
    let
      applySign = if sign == Just '-' then negate else id
      base prefix digits readBase hint desc = fmap ((,) hint . readBase)
        $ char prefix *> many1 (oneOf digits <?> (desc ++ " digit"))
    choice [
      try . fmap (\ (hint, value) -> IntegerToken (applySign value) hint) $ char '0' *> choice
        [ base 'b' "01" readBin Binary "binary"
        , base 'o' ['0'..'7'] (fst . head . readOct) Octal "octal"
        , base 'x' (['0'..'9'] ++ ['A'..'F']) (fst . head . readHex) Hexadecimal "hexadecimal"
        ],
      do
        integer <- many1 digit
        mFraction <- optionMaybe $ (:) <$> char '.' <*> many1 digit
        return $ case mFraction of
          Just fraction -> FloatToken . applySign . read $ integer ++ fraction
          Nothing -> IntegerToken (applySign (read integer)) Decimal ]
      <* notFollowedBy digit,
  try $ ArrowToken <$ string "->" <* notFollowedBy symbol,
  let
    alphanumeric = (Text.pack .) . (:) <$> (letter <|> char '_') <*> (many . choice) [letter, char '_', digit]
    symbolic = Name Relative . (:[]) . Text.pack <$> many1 symbol
    in choice [
      flip fmap alphanumeric $ \name -> case name of
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
        _ -> WordToken (Name Relative [name]),
      OperatorToken <$> symbolic ] ]
  where
  character quote = (Just <$> noneOf ('\\' : [quote])) <|> escape
  escape = char '\\' *> choice [
    Just <$> oneOf "\\\"'",
    Just '\a' <$ char 'a',
    Just '\b' <$ char 'b',
    Just '\f' <$ char 'f',
    Just '\n' <$ char 'n',
    Just '\r' <$ char 'r',
    Just '\t' <$ char 't',
    Just '\v' <$ char 'v',
    Just <$> (space <* spaces),
    Nothing <$ char '&' ]
  letter = Parsec.satisfy isLetter
  readBin = go 0
    where
    go :: Integer -> [Char] -> Integer
    go acc ds = case ds of
      '0' : ds' -> go (2 * acc + 0) ds'
      '1' : ds' -> go (2 * acc + 1) ds'
      [] -> acc
      _ -> error "non-binary digit"
  special = oneOf "\"'(),:[\\]_{}"
  symbol = notFollowedBy special *> choice [satisfy isSymbol, satisfy isPunctuation]
  text = Text.pack . catMaybes <$> many (character '"')

layout :: FilePath -> [Token] -> K [Token]
layout path tokens = case Parsec.runParser insertBraces (Name Absolute []) path tokens of
  Left parseError -> fail (show parseError)
  Right result -> return result

insertBraces :: Parser [Token]
insertBraces = (concat <$> many unit) <* eof
  where

  unit :: Parser [Token]
  unit = unitWhere $ const True

  unitWhere :: (Token -> Bool) -> Parser [Token]
  unitWhere predicate
    = try (lookAhead $ tokenSatisfy predicate) *> choice [
    bracket (BlockBeginToken Nonlayout) BlockEndToken,
    bracket GroupBeginToken GroupEndToken,
    bracket VectorBeginToken VectorEndToken,
    layoutBlock,
    (:[]) <$> tokenSatisfy nonbracket ]

  bracket :: (Origin -> Indent -> Token) -> (Origin -> Indent -> Token) -> Parser [Token]
  bracket open close = do
    begin <- parserMatch open
    inner <- concat <$> many unit
    end <- parserMatch close
    return (begin : inner ++ [end])

  nonbracket :: Token -> Bool
  nonbracket = not . (`elem` brackets)

  brackets :: [Token]
  brackets = blockBrackets ++ [
    GroupBeginToken Anywhere Nothing,
    GroupEndToken Anywhere Nothing,
    VectorBeginToken Anywhere Nothing,
    VectorEndToken Anywhere Nothing ]

  blockBrackets :: [Token]
  blockBrackets = [
    BlockBeginToken Nonlayout Anywhere Nothing,
    BlockEndToken Anywhere Nothing,
    LayoutToken Anywhere Nothing ]

  layoutBlock :: Parser [Token]
  layoutBlock = do
    colon <- parserMatch LayoutToken
    let
      colonOrigin = tokenOrigin colon
      colonIndent = tokenIndent colon
      validFirst token = let
        column = tokenIndent token
        in column > Just 1 && column >= colonIndent
    first <- lookAhead (tokenSatisfy validFirst)
      <?> "token indented no less than start of layout block"
    let
      firstOrigin = rangeBegin (tokenOrigin first)
      inside token = sourceColumn (rangeBegin (tokenOrigin token)) >= sourceColumn firstOrigin
    body <- concat <$> many (unitWhere inside)
    return (BlockBeginToken Layout colonOrigin colonIndent : body ++ [BlockEndToken colonOrigin colonIndent])

tokenSatisfy :: (Token -> Bool) -> Parser Token
tokenSatisfy predicate = tokenPrim show advance
  $ \token -> if predicate token then Just token else Nothing
  where
  advance :: SourcePos -> t -> [Token] -> SourcePos
  advance _ _ (token : _) = rangeBegin (tokenOrigin token)
  advance sourcePos _ _ = sourcePos

parserMatch :: (Origin -> Indent -> Token) -> Parser Token
parserMatch token = tokenSatisfy (== token Anywhere Nothing) <?> show (token Anywhere Nothing)

parserMatch_ :: (Origin -> Indent -> Token) -> Parser ()
parserMatch_ = void . parserMatch

parse :: FilePath -> [Token] -> K Fragment
parse name tokens = case Parsec.runParser fragmentParser (Name Absolute []) name tokens of
  Left parseError -> fail (show parseError)
  Right result -> return result

data Fragment = Fragment {
  fragmentDataDefinitions :: [DataDefinition],
  fragmentDefinitions :: [Definition],
  fragmentOperators :: [Operator],
  fragmentSynonyms :: [Synonym] }
  deriving (Show)

instance Monoid Fragment where
  mempty = Fragment {
    fragmentDataDefinitions = [],
    fragmentDefinitions = [],
    fragmentOperators = [],
    fragmentSynonyms = [] }
  mappend a b = Fragment {
    fragmentDataDefinitions = fragmentDataDefinitions a ++ fragmentDataDefinitions b,
    fragmentDefinitions = fragmentDefinitions a ++ fragmentDefinitions b,
    fragmentOperators = fragmentOperators a ++ fragmentOperators b,
    fragmentSynonyms = fragmentSynonyms a ++ fragmentSynonyms b }

fragmentParser :: Parser Fragment
fragmentParser = do
  partitionElements . concat <$> many (vocabularyParser <|> (:[]) <$> elementParser) <* eof

partitionElements :: [Element] -> Fragment
partitionElements = foldr go (Fragment [] [] [] [])
  where
  go element acc = case element of
    DataDefinitionElement x -> acc { fragmentDataDefinitions = x : fragmentDataDefinitions acc }
    DefinitionElement x -> acc { fragmentDefinitions = x : fragmentDefinitions acc }
    OperatorElement x -> acc { fragmentOperators = x : fragmentOperators acc }
    SynonymElement x -> acc { fragmentSynonyms = x : fragmentSynonyms acc }

vocabularyParser :: Parser [Element]
vocabularyParser = do
  parserMatch_ VocabToken
  prefix <- getState
  suffix <- nameParser
  putState (joinNames prefix suffix)
  choice [
    [] <$ parserMatch (OperatorToken ";"),
    do
      elements <- blockedParser (many elementParser)
      putState prefix
      return elements ]

blockedParser :: Parser a -> Parser a
blockedParser = between (parserMatch (BlockBeginToken Nonlayout)) (parserMatch BlockEndToken)

groupedParser :: Parser a -> Parser a
groupedParser = between (parserMatch GroupBeginToken) (parserMatch GroupEndToken)

groupParser :: Parser Term
groupParser = do
  origin <- getOrigin
  groupedParser (compose origin <$> many1 termParser)

angledParser :: Parser a -> Parser a
angledParser = between (parserMatch (OperatorToken "<")) (parserMatch (OperatorToken ">"))

nameParser :: Parser Name
nameParser = do
  global <- isJust <$> optionMaybe (parserMatch IgnoreToken <* parserMatch VocabLookupToken)
  parts <- concatMap nameParts <$> (unqualifiedNameParser `sepBy1` parserMatch VocabLookupToken)
  return (Name (if global then Absolute else Relative) parts)

unqualifiedNameParser :: Parser Name
unqualifiedNameParser = wordNameParser <|> operatorNameParser

wordNameParser :: Parser Name
wordNameParser = parseOne $ \x -> case x of
  WordToken name _ _ -> Just name
  _ -> Nothing

operatorNameParser :: Parser Name
operatorNameParser = parseOne $ \x -> case x of
  OperatorToken name _ _ -> Just name
  _ -> Nothing

parseOne :: (Token -> Maybe a) -> Parser a
parseOne = tokenPrim show advance
  where
  advance :: SourcePos -> t -> [Token] -> SourcePos
  advance _ _ (token : _) = rangeBegin (tokenOrigin token)
  advance sourcePos _ _ = sourcePos

elementParser :: Parser Element
elementParser = choice [
  DataDefinitionElement <$> dataDefinitionParser,
  DefinitionElement <$> definitionParser,
  OperatorElement <$> operatorParser,
  SynonymElement <$> synonymParser ]

synonymParser :: Parser Synonym
synonymParser = (<?> "synonym") $ do
  origin <- getOrigin <* parserMatch_ SynonymToken
  prefix <- getState
  suffix <- (wordNameParser <|> operatorNameParser)
  let from = joinNames prefix suffix
  to <- nameParser
  return (Synonym from to origin)

operatorParser :: Parser Operator
operatorParser = do
  parserMatch_ InfixToken
  (associativity, precedence) <- choice [
    (Nonassociative,) <$> precedenceParser,
    (Leftward,) <$> (parserMatch_ (WordToken "left") *> precedenceParser),
    (Rightward,) <$> (parserMatch_ (WordToken "right") *> precedenceParser) ]
  prefix <- getState
  suffix <- operatorNameParser
  let name = joinNames prefix suffix
  return Operator {
    operatorAssociativity = associativity,
    operatorName = name,
    operatorPrecedence = precedence }
  where
  precedenceParser :: Parser Precedence
  precedenceParser = (<?> "decimal integer precedence from 0 to 9") $ parseOne $ \token -> case token of
    IntegerToken value Decimal _ _
      | value >= 0 && value <= 9 -> Just (fromInteger value)
    _ -> Nothing

dataDefinitionParser :: Parser DataDefinition
dataDefinitionParser = (<?> "data definition") $ do
  origin <- getOrigin <* parserMatch DataToken
  prefix <- getState
  suffix <- wordNameParser <?> "data definition name"
  let name = joinNames prefix suffix
  parameters <- option [] quantifierParser
  constructors <- blockedParser (many (constructorParser name))
  return DataDefinition {
    dataConstructors = constructors,
    dataName = name,
    dataOrigin = origin,
    dataParameters = parameters }

constructorParser :: Name -> Parser DataConstructor
constructorParser prefix = (<?> "data constructor") $ do
  origin <- getOrigin <* parserMatch CaseToken
  suffix <- wordNameParser <?> "constructor name"
  fields <- option [] (groupedParser (typeParser `sepEndBy` parserMatch CommaToken))
  return DataConstructor {
    constructorFields = fields,
    constructorName = joinNames prefix suffix,
    constructorOrigin = origin }

typeParser :: Parser Signature
typeParser = try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  (stackEffect, origin) <- choice [
    try $ do
      leftVar <- stack; leftTypes <- left
      origin <- arrow
      rightVar <- stack; rightTypes <- right
      return (StackFunctionSignature leftVar leftTypes rightVar rightTypes, origin),
    do
      leftTypes <- left
      origin <- arrow
      rightTypes <- right
      return (FunctionSignature leftTypes rightTypes, origin) ]
  sideEffects <- many (parserMatch (OperatorToken "+") *> wordNameParser)
  return $ stackEffect sideEffects origin
  where
  stack = wordNameParser <* parserMatch EllipsisToken
  left = basicTypeParser `sepEndBy1` parserMatch CommaToken
  right = typeParser `sepEndBy1` parserMatch CommaToken
  arrow = getOrigin <* parserMatch ArrowToken

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  foldl1' (\ a b -> ApplicationSignature a b (signatureOrigin a)) <$> many1 (choice [
    quantifiedParser (groupedParser typeParser),
    do
      origin <- getOrigin
      SignatureVariable <$> wordNameParser <*> pure origin,
    groupedParser typeParser ])

quantifierParser :: Parser [(Name, Kind, Origin)]
quantifierParser = angledParser (var `sepEndBy1` parserMatch CommaToken)
  where
  var = do
    origin <- getOrigin
    choice [
      (, Effect, origin) <$> (parserMatch (OperatorToken "+") *> wordNameParser),
      do
        name <- wordNameParser
        (name,, origin) <$> option Value (Stack <$ parserMatch EllipsisToken) ]

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getOrigin
  QuantifiedSignature <$> quantifierParser <*> thing <*> pure origin

definitionParser :: Parser Definition
definitionParser = (<?> "definition") $ do
  origin <- getOrigin <* parserMatch DefineToken
  (fixity, suffix) <- choice [(Postfix,) <$> wordNameParser, (Infix,) <$> operatorNameParser] <?> "definition name"
  signature <- signatureParser
  body <- choice [blockParser, blockLambdaParser] <?> "definition body"
  prefix <- getState
  let name = joinNames prefix suffix
  return Definition {
    definitionBody = body,
    definitionFixity = fixity,
    definitionName = name,
    definitionOrigin = origin,
    definitionSignature = signature }

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
  return (foldr (Compose Nothing) (Identity Nothing origin') terms)

termParser :: Parser Term
termParser = do
  origin <- getOrigin
  choice [
    try (uncurry (Push Nothing) <$> parseOne toLiteral <?> "literal"),
    Call Nothing Infix <$> operatorNameParser <*> pure origin,
    Call Nothing Postfix <$> wordNameParser <*> pure origin,
    try sectionParser,
    try groupParser <?> "parenthesized expression",
    lambdaParser,
    matchParser,
    ifParser,
    Push Nothing <$> blockValue <*> pure origin ]
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
  sectionParser = (<?> "operator section") $ groupedParser $ choice [
    do
      origin <- getOrigin
      function <- operatorNameParser
      choice [
        do
          operandOrigin <- getOrigin
          operand <- many1 termParser
          return $ compose operandOrigin $ operand ++ [Call Nothing Postfix function origin],
        return (Call Nothing Postfix function origin) ],
    do
      operandOrigin <- getOrigin
      operand <- many1 (notFollowedBy operatorNameParser *> termParser)
      origin <- getOrigin
      function <- operatorNameParser
      return $ compose operandOrigin $ operand ++ [Swap Nothing origin, Call Nothing Postfix function origin] ]
  lambdaParser :: Parser Term
  lambdaParser = (<?> "variable introduction") $ choice [
    try $ parserMatch ArrowToken *> do
      names <- lambdaNamesParser <* parserMatch (OperatorToken ";")
      origin <- getOrigin
      body <- blockContentsParser
      return $ makeLambda names body origin,
    do
      body <- blockLambdaParser
      let origin = termOrigin body
      return $ Push Nothing (Quotation body) origin ]
  matchParser :: Parser Term
  matchParser = (<?> "match") $ do
    matchOrigin <- getOrigin <* parserMatch MatchToken
    scrutineeOrigin <- getOrigin
    mScrutinee <- optionMaybe groupParser <?> "scrutinee"
    (cases, mElse) <- blockedParser $ do
      cases' <- many $ (<?> "case") $ parserMatch CaseToken *> do
        origin <- getOrigin
        name <- nameParser
        body <- choice [blockParser, blockLambdaParser]
        return $ Case name body origin
      mElse' <- optionMaybe $ do
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
    mCondition <- optionMaybe groupParser <?> "condition"
    ifBody <- blockParser
    elifs <- many $ do
      origin <- getOrigin <* parserMatch ElifToken
      condition <- groupParser <?> "condition"
      body <- blockParser
      return (condition, body, origin)
    mElse <- optionMaybe $ do
      origin <- getOrigin <* parserMatch ElseToken
      body <- blockParser
      return $ (Push Nothing (Boolean True) origin, body, origin)
    return $ foldr
      (\ (condition, body, origin) acc -> compose ifOrigin [condition, If Nothing body acc origin])
      (Identity Nothing ifOrigin)
      ((fromMaybe (Identity Nothing ifOrigin) mCondition, ifBody, ifOrigin)
        : elifs ++ maybeToList mElse)
  blockValue :: Parser Value
  blockValue = (<?> "quotation") $ do
    origin <- getOrigin
    let
      reference = Call Nothing Postfix
        <$> (parserMatch ReferenceToken *> nameParser)
        <*> pure origin
    Quotation <$> (blockParser <|> reference)

lambdaBlockParser :: Parser ([(Maybe Name, Origin)], Term, Origin)
lambdaBlockParser = parserMatch ArrowToken *> do
  names <- lambdaNamesParser
  origin <- getOrigin
  body <- blockParser
  return (names, body, origin)

lambdaNamesParser :: Parser [(Maybe Name, Origin)]
lambdaNamesParser = many1 $ do
  origin <- getOrigin
  name <- Just <$> wordNameParser <|> Nothing <$ parserMatch IgnoreToken
  return (name, origin)

blockLambdaParser :: Parser Term
blockLambdaParser = do
  (names, body, origin) <- lambdaBlockParser
  return $ makeLambda names body origin

makeLambda :: [(Maybe Name, Origin)] -> Term -> Origin -> Term
makeLambda parsed body origin = foldr
  (\ (nameMaybe, nameOrigin) acc -> maybe
    (Compose Nothing (Drop Nothing origin) acc)
    (\ name -> Compose Nothing (Go Nothing name nameOrigin) acc)
    nameMaybe)
  body
  (reverse parsed)

getOrigin :: (Monad m, Stream s m c) => ParsecT s u m Origin
getOrigin = do
  start <- getPosition
  return (Range start start)

data Element
  = DataDefinitionElement !DataDefinition
  | DefinitionElement !Definition
  | OperatorElement !Operator
  | SynonymElement !Synonym

data Definition = Definition {
  definitionBody :: !Term,
  definitionFixity :: !Fixity,
  definitionName :: !Name,
  definitionOrigin :: !Origin,
  definitionSignature :: !Signature }
  deriving (Show)

data DataDefinition = DataDefinition {
  dataConstructors :: [DataConstructor],
  dataName :: !Name,
  dataOrigin :: !Origin,
  dataParameters :: [(Name, Kind, Origin)] }
  deriving (Show)

data DataConstructor = DataConstructor {
  constructorFields :: [Signature],
  constructorName :: !Name,
  constructorOrigin :: !Origin }
  deriving (Show)

data Operator = Operator {
  operatorAssociativity :: !Associativity,
  operatorName :: !Name,
  operatorPrecedence :: !Precedence }
  deriving (Show)

data Fixity = Infix | Postfix
  deriving (Eq, Show)

data Synonym = Synonym !Name !Name !Origin
  deriving (Show)

data Associativity = Nonassociative | Leftward | Rightward
  deriving (Show)

type Precedence = Int

data Term
  = Call !(Maybe Type) !Fixity !Name !Origin
  | Compose !(Maybe Type) !Term !Term
  | Come !HowCome !(Maybe Type) !Name !Origin
  | Drop !(Maybe Type) !Origin
  | Generic !TypeIdentifier !Term !Origin
  | Go !(Maybe Type) !Name !Origin
  | Identity !(Maybe Type) !Origin
  | If !(Maybe Type) !Term !Term !Origin
  | Match !(Maybe Type) [Case] !(Maybe Else) !Origin
  | Push !(Maybe Type) !Value !Origin
  | Swap !(Maybe Type) !Origin
  deriving (Show)

compose :: Origin -> [Term] -> Term
compose origin = foldr (Compose Nothing) (Identity Nothing origin)

termOrigin :: Term -> Origin
termOrigin term = case term of
  Call _ _ _ origin -> origin
  Compose _ a _ -> termOrigin a
  Come _ _ _ origin -> origin
  Drop _ origin -> origin
  Generic _ _ origin -> origin
  Go _ _ origin -> origin
  Identity _ origin -> origin
  If _ _ _ origin -> origin
  Match _ _ _ origin -> origin
  Push _ _ origin -> origin
  Swap _ origin -> origin

data Value
  = Boolean !Bool
  | Character !Char
  | Closed !ClosureIndex
  | Closure [Closed]
  | Float !Double
  | Integer !Integer
  | Local !LocalIndex
  | Quotation !Term
  | Text !Text
  deriving (Show)

data Case = Case !Name !Term !Origin
  deriving (Show)

data Else = Else !Term !Origin
  deriving (Show)

data HowCome = Copy | Move
  deriving (Show)

data Signature
  = ApplicationSignature Signature Signature !Origin
  | FunctionSignature [Signature] [Signature] [Name] !Origin
  | QuantifiedSignature [(Name, Kind, Origin)] !Signature !Origin
  | SignatureVariable !Name !Origin
  | StackFunctionSignature !Name [Signature] !Name [Signature] [Name] !Origin
  deriving (Show)

signatureOrigin :: Signature -> Origin
signatureOrigin signature = case signature of
  ApplicationSignature _ _ origin -> origin
  FunctionSignature _ _ _ origin -> origin
  QuantifiedSignature _ _ origin -> origin
  SignatureVariable _ origin -> origin
  StackFunctionSignature _ _ _ _ _ origin -> origin

data Type
  = !Type :@ !Type
  | TypeConstructor !Constructor
  | TypeVariable !Variable
  | TypeConstant !Variable
  | Forall !Variable !Type
 deriving (Eq, Show)

data TypeIdentifier = TypeIdentifier !Int
  deriving (Eq, Show)

data Variable = Variable !TypeIdentifier !Kind
  deriving (Eq, Show)

data Kind = Value | Stack | Label | Effect | !Kind :-> !Kind
  deriving (Eq, Show)

newtype Constructor = Constructor Name
  deriving (Eq, Show)

type LocalIndex = Int

type ClosureIndex = Int

data Closed = ClosedLocal !LocalIndex | ClosedClosure !ClosureIndex
  deriving (Show)

data Limit = Unlimited | Limit0 | Limit1
  deriving (Show)

type Error = String

data Env = Env { envErrors :: IORef [Error] }

newtype K a = K { runK :: Env -> IO (Maybe a) }

instance Functor K where
  fmap f (K ax) = K $ fmap (fmap f) . ax

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
  fail = (>> halt) . err

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
  errors <- readIORef (envErrors env)
  return $ if null errors then Just () else Nothing

err :: Error -> K ()
err e = K $ \env -> Just <$> modifyIORef (envErrors env) (e :)

halt :: K a
halt = K $ const $ return Nothing

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile

type Indent = Maybe Column

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
  | OperatorToken !Name !Origin !Indent          -- +
  | ReferenceToken !Origin !Indent               -- \
  | SynonymToken !Origin !Indent                 -- synonym
  | TextToken !Text !Origin !Indent              -- "..."
  | VectorBeginToken !Origin !Indent             -- [
  | VectorEndToken !Origin !Indent               -- ]
  | VocabToken !Origin !Indent                   -- vocab
  | VocabLookupToken !Origin !Indent             -- ::
  | WordToken !Name !Origin !Indent              -- word

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
  _                     == _                   = False

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
  ArrowToken _ indent        -> indent
  BlockBeginToken _ _ indent -> indent
  BlockEndToken _ indent     -> indent
  BooleanToken _ _ indent    -> indent
  CaseToken _ indent         -> indent
  CharacterToken _ _ indent  -> indent
  CommaToken _ indent        -> indent
  DataToken _ indent         -> indent
  DefineToken _ indent       -> indent
  EllipsisToken _ indent     -> indent
  ElifToken _ indent         -> indent
  ElseToken _ indent         -> indent
  FloatToken _ _ indent      -> indent
  GroupBeginToken _ indent   -> indent
  GroupEndToken _ indent     -> indent
  IfToken _ indent           -> indent
  IgnoreToken _ indent       -> indent
  InfixToken _ indent        -> indent
  IntegerToken _ _ _ indent  -> indent
  LayoutToken _ indent       -> indent
  MatchToken _ indent        -> indent
  OperatorToken _ _ indent   -> indent
  ReferenceToken _ indent    -> indent
  SynonymToken _ indent      -> indent
  TextToken _ _ indent       -> indent
  VectorBeginToken _ indent  -> indent
  VectorEndToken _ indent    -> indent
  VocabToken _ indent        -> indent
  VocabLookupToken _ indent  -> indent
  WordToken _ _ indent       -> indent

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
      (base :: Integer, prefix :: String, digits) = case hint of
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

data Layoutness = Layout | Nonlayout
  deriving (Show)

data Base = Binary | Octal | Decimal | Hexadecimal
  deriving (Show)

-- A token origin.
data Origin
  = Range {
    rangeBegin :: !SourcePos,
    rangeEnd :: !SourcePos }
  | Anywhere
  deriving (Show)

{-
instance Show Origin where
  show (Range a b n) = concat
    $ [sourceName a, ":", show al, ".", show ac, "-"]
    ++ (if al == bl then [show bc] else [show bl, ".", show bc])
    ++ [": indent ", show n]
    where
    al = sourceLine a
    bl = sourceLine b
    ac = sourceColumn a
    bc = sourceColumn b - 1
  show _ = ""
-}

data Root = Absolute | Relative
  deriving (Eq, Ord)

data Name = Name { nameRoot :: !Root, nameParts :: [Text] }
  deriving (Eq, Ord)

joinNames :: Name -> Name -> Name
joinNames (Name root as) (Name Relative bs) = Name root (as ++ bs)
joinNames _ (Name Absolute _) = error "joining absolute names"

nameVocabulary :: Name -> Name
nameVocabulary (Name _ []) = Name Absolute []
nameVocabulary (Name root parts) = Name root (init parts)

nameRelative :: Name -> Name
nameRelative (Name _ []) = error "cannot convert global vocab to relative"
nameRelative (Name _ parts) = Name Relative (tail parts)

instance Show Name where
  show (Name Relative parts) = Text.unpack (Text.intercalate "::" parts)
  show (Name Absolute parts) = Text.unpack (Text.intercalate "::" ("_" : parts))

instance IsString Name where
  fromString s = Name Relative [fromString s]

skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
a `skipManyTill` b = void (try b) <|> a *> (a `skipManyTill` b)
