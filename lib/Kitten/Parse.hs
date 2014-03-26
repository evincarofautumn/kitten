{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse
  ( parse
  , rewriteInfix
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.Vector as V
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec as Parsec

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Import
import Kitten.Location
import Kitten.Operator
import Kitten.Parsec
import Kitten.Parse.Element
import Kitten.Parse.Layout
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parse.Type
import Kitten.Token (Located(..), Token)
import Kitten.Tree
import Kitten.Type (StackHint(..), mono)
import Kitten.Util.Either
import Kitten.Util.Function
import Kitten.Util.Maybe
import Kitten.Util.Parsec

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Token as Token

parse
  :: String
  -> [Located]
  -> Either ErrorGroup (Fragment ParsedTerm)
parse name tokens = mapLeft parseError $ do
  inserted <- Parsec.parse insertBraces name tokens
  Parsec.parse (setInitialPosition name inserted >> fragment) name inserted

setInitialPosition :: String -> [Located] -> Parser ()
setInitialPosition name [] = setPosition (newPos name 1 1)
setInitialPosition _ (Located{..} : _)
  = setPosition (locationStart locatedLocation)

fragment :: Parser (Fragment ParsedTerm)
fragment = fmap (partitionElements . concat)
  $ many element `sepEndBy` match Token.Semicolon <* eof

element :: Parser Element
element = choice
  [ DefElement <$> def
  , ImportElement <$> import_
  , OperatorElement <$> operatorDeclaration
  , TermElement <$> term
  ]

def :: Parser (Def ParsedTerm)
def = (<?> "definition") . locate $ do
  void (match Token.Def)
  (hint, name) <- choice
    [ (,) PostfixHint <$> named
    , (,) InfixHint <$> symbolic
    ] <?> "definition name"
  anno <- signature
  bodyTerm <- locate (Compose StackAny <$> block)
    <?> "definition body"
  return $ \loc -> Def
    { defAnno = anno
    , defFixityHint = hint
    , defLocation = loc
    , defName = name
    , defTerm = mono bodyTerm
    }

import_ :: Parser Import
import_ = (<?> "import") . locate $ do
  void (match Token.Import)
  name <- named
  return $ \loc -> Import
    { importName = name
    , importLocation = loc
    }

operatorDeclaration :: Parser Operator
operatorDeclaration = (<?> "operator declaration") . locate $ uncurry Operator
  <$> choice
    [ match Token.Infix *> ((,) Infix <$> precedence)
    , match Token.InfixLeft *> ((,) InfixLeft <$> precedence)
    , match Token.InfixRight *> ((,) InfixRight <$> precedence)
    , (Postfix, 9) <$ match Token.Postfix
    , (Prefix, 9) <$ match Token.Prefix
    ]
  <*> symbolic
  where
  precedence = (<?> "decimal integer precedence from 0 to 9")
    $ mapOne $ \case
      Token.Int value Token.DecimalHint
        | value >= 0 && value <= 9 -> Just value
      _ -> Nothing

term :: Parser ParsedTerm
term = nonblockTerm <|> blockTerm
  where
  nonblockTerm :: Parser ParsedTerm
  nonblockTerm = locate $ choice
    [ try $ Push <$> locate (mapOne toLiteral <?> "literal")
    , Call PostfixHint <$> named
    , Call InfixHint <$> symbolic
    , try section
    , try group <?> "grouped expression"
    , pair <$> tuple
    , VectorTerm <$> vector
    , mapOne toBuiltin <?> "builtin"
    , lambda
    , doElse
    ]

  section :: Parser (Location -> ParsedTerm)
  section = (<?> "operator section") $ grouped $ choice
    [ do
      void $ match Token.Ignore
      function <- symbolic
      choice
        [ do
          operand <- many1V term
          return $ \loc -> Compose StackAny (V.fromList
            [ Compose Stack1 operand loc
            , Call PostfixHint function loc
            ]) loc
        , do
          void $ match Token.Ignore
          return $ Call PostfixHint function
        ]
    , do
      function <- symbolic
      return $ Call PostfixHint function
    , do
      operand <- many1V (notFollowedBy symbolic *> term)
      function <- symbolic
      void $ match Token.Ignore
      return $ \loc -> Compose StackAny (V.fromList
        [ Compose Stack1 operand loc
        , swap loc
        , Call PostfixHint function loc
        ]) loc
    ]
    where
    swap loc = Lambda "right operand" (Lambda "left operand"
      (Compose StackAny (V.fromList
        [ Call PostfixHint "right operand" loc
        , Call PostfixHint "left operand" loc
        ]) loc) loc) loc

  blockTerm :: Parser ParsedTerm
  blockTerm = locate $ Push <$> blockValue

  group :: Parser (Location -> ParsedTerm)
  group = (<?> "group") $ Compose StackAny <$> grouped (many1V term)

  doElse :: Parser (Location -> ParsedTerm)
  doElse = (<?> "do block") $ match Token.Do *> doBody

  toFunctionOrBuiltin :: Token -> Maybe Text
  toFunctionOrBuiltin token = case token of
    Token.Builtin name -> Just (Builtin.toText name)
    Token.Word name -> Just name
    Token.Operator name -> Just name
    _ -> Nothing

  doBody :: Parser (Location -> ParsedTerm)
  doBody = (<?> "do body") $ do
    name <- mapOne toFunctionOrBuiltin
    open <- many nonblockTerm
    body <- blockTerm
    else_ <- optionMaybe $ match Token.Else *> choice
      [ try . locate $ Push
        <$> locate (Quotation <$> locate doBody)
      , blockTerm
      ]
    let
      (name', else') = case else_ of
        Nothing -> (name, V.empty)
        Just elseBody -> (name <> "_else", V.singleton elseBody)
      name'' = case Builtin.fromText name' of
        Just builtin -> Builtin builtin
        _ -> Call PostfixHint name'
    return $ \loc -> let
      whole = V.concat
        [ V.fromList [Compose StackAny (V.fromList open) loc, body]
        , else'
        , V.singleton (name'' loc)
        ]
      in Compose StackAny whole loc

  lambda :: Parser (Location -> ParsedTerm)
  lambda = (<?> "lambda") $ match Token.Arrow *> do
    names <- many1 $ Just <$> named <|> Nothing <$ match Token.Ignore
    void (match Token.Semicolon)
    terms <- blockContents
    return $ \loc -> foldr
      (\mLambdaName lambdaTerms -> maybe
        (Compose StackAny (V.fromList
          [ Lambda "ignored" (Compose StackAny V.empty loc) loc
          , lambdaTerms
          ]) loc)
        (\lambdaName -> Lambda lambdaName lambdaTerms loc)
        mLambdaName)
      (Compose StackAny terms loc)
      (reverse names)

  pair :: Vector ParsedTerm -> Location -> ParsedTerm
  pair values loc = V.foldr1 (\x y -> PairTerm x y loc) values

  toBuiltin :: Token -> Maybe (Location -> ParsedTerm)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser (Vector ParsedTerm)
  tuple = grouped
    (locate (Compose StackAny <$> many1V term)
      `sepEndBy1V` match Token.Comma)
    <?> "tuple"

  vector :: Parser (Vector ParsedTerm)
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose StackAny <$> many1V term)
      `sepEndByV` match Token.Comma)
    <?> "vector"

blockValue :: Parser ParsedValue
blockValue = (<?> "function") . locate $ do
  terms <- block
  return $ \loc -> Quotation (Compose StackAny terms loc) loc

toLiteral :: Token -> Maybe (Location -> ParsedValue)
toLiteral (Token.Bool x) = Just $ Bool x
toLiteral (Token.Char x) = Just $ Char x
toLiteral (Token.Float x) = Just $ Float x
toLiteral (Token.Int x _) = Just $ Int x
toLiteral (Token.Text x) = Just $ String x
toLiteral _ = Nothing

block :: Parser (Vector ParsedTerm)
block = blocked blockContents <?> "block"

blockContents :: Parser (Vector ParsedTerm)
blockContents = locate $ do
  void $ optional semi
  groups <- many1V term `sepEndBy` semi
  return $ \loc -> V.fromList $ map (\t -> Compose StackAny t loc) groups
  where semi = match Token.Semicolon

----------------------------------------

type TermParser a = ParsecT [ParsedTerm] () Identity a

rewriteInfix
  :: Fragment ParsedTerm
  -> Either ErrorGroup (Fragment ParsedTerm)
rewriteInfix parsed@Fragment{..} = do
  defs <- V.mapM (traverse rewriteInfixTerm) fragmentDefs
  terms <- V.mapM rewriteInfixTerm fragmentTerms
  return parsed
    { fragmentDefs = defs
    , fragmentTerms = terms
    }

  where
  rewriteInfixTerm :: ParsedTerm -> Either ErrorGroup ParsedTerm
  rewriteInfixTerm = \case
    x@Builtin{} -> return x
    x@Call{} -> return x
    Compose hint terms loc -> do
      terms' <- V.toList <$> V.mapM rewriteInfixTerm terms
      let
        expression' = between (setPositionTerm loc terms') eof infixExpression
        infixExpression = Compose hint
          <$> manyV (expression <|> lambdaTerm)
          <*> pure loc
          <?> "infix expression"
      case Parsec.parse expression' [] terms' of
        Left message -> Left $ parseError message
        Right parseResult -> Right parseResult
    Lambda name body loc -> do
      body' <- rewriteInfixTerm body
      return $ Lambda name body' loc
    PairTerm a b loc -> do
      a' <- rewriteInfixTerm a
      b' <- rewriteInfixTerm b
      return $ PairTerm a' b' loc
    Push value loc -> do
      value' <- rewriteInfixValue value
      return $ Push value' loc
    VectorTerm terms loc -> do
      terms' <- V.mapM rewriteInfixTerm terms
      return $ VectorTerm terms' loc

  lambdaTerm :: TermParser ParsedTerm
  lambdaTerm = satisfyTerm $ \case
    Lambda{} -> True
    _ -> False

  rewriteInfixValue :: ParsedValue -> Either ErrorGroup ParsedValue
  rewriteInfixValue = \case
    Quotation body loc -> Quotation <$> rewriteInfixTerm body <*> pure loc
    -- TODO Exhaustivity/safety.
    other -> return other

  stack1 :: Location -> ParsedTerm -> ParsedTerm
  stack1 loc x = Compose Stack1 (V.singleton x) loc

  binary :: Text -> Location -> ParsedTerm -> ParsedTerm -> ParsedTerm
  binary name loc x y = Compose StackAny (V.fromList
    [ stack1 loc x
    , stack1 loc y
    , Call InfixHint name loc
    ]) loc

  unary :: Text -> Location -> ParsedTerm -> ParsedTerm
  unary name loc x = Compose StackAny (V.fromList
    [ stack1 loc x
    , Call InfixHint name loc
    ]) loc

  binaryOp :: Text -> TermParser (ParsedTerm -> ParsedTerm -> ParsedTerm)
  binaryOp name = mapTerm $ \t -> case t of
    Call InfixHint name' loc
      | name == name' -> Just (binary name loc)
    _ -> Nothing

  unaryOp :: Text -> TermParser (ParsedTerm -> ParsedTerm)
  unaryOp name = mapTerm $ \case
    Call InfixHint name' loc | name == name' -> Just (unary name loc)
    _ -> Nothing

  expression :: TermParser ParsedTerm
  expression = E.buildExpressionParser opTable
    (locate (Compose StackAny <$> many1V operand) <?> "operand")
    where
    operand :: TermParser ParsedTerm
    operand = satisfyTerm $ \case
      Call InfixHint _ _ -> False  -- An operator is not an operand.
      Lambda{} -> False  -- Nor is a bare lambda.
      _ -> True

  -- TODO Detect/report duplicates.
  opTable = map (map toOp) rawTable

  rawTable :: [[Operator]]
  rawTable = let
    -- TODO Make smarter than a linear search.
    useDefault op = defFixityHint op == InfixHint
      && not (any ((defName op ==) . operatorName) fragmentOperators)
    flat = fragmentOperators
      ++ map (\Def{..} -> Operator InfixLeft 6 defName defLocation)
        (filter useDefault (V.toList fragmentDefs))
    in for [9,8..0] $ \p -> filter ((p ==) . operatorPrecedence) flat

  toOp :: Operator -> E.Operator [ParsedTerm] () Identity ParsedTerm
  toOp (Operator fixity _ name _) = case fixity of
    Infix -> E.Infix (binaryOp name) E.AssocNone
    InfixLeft -> E.Infix (binaryOp name) E.AssocLeft
    InfixRight -> E.Infix (binaryOp name) E.AssocRight
    Prefix -> E.Prefix (unaryOp name)
    Postfix -> E.Postfix (unaryOp name)

mapTerm :: (ParsedTerm -> Maybe a) -> TermParser a
mapTerm = tokenPrim show advanceTerm

advanceTerm :: SourcePos -> t -> [ParsedTerm] -> SourcePos
advanceTerm _ _ (t : _) = locationStart (termMetadata t)
advanceTerm sourcePos _ _ = sourcePos

satisfyTerm :: (ParsedTerm -> Bool) -> TermParser ParsedTerm
satisfyTerm predicate = tokenPrim show advanceTerm
  $ \t -> justIf (predicate t) t

setPositionTerm :: Location -> [ParsedTerm] -> TermParser ()
setPositionTerm _ (t : _) = setPosition (locationStart (termMetadata t))
setPositionTerm loc [] = setPosition (locationStart loc)
