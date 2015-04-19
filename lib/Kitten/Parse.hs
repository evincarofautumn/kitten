{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Parse
  ( parse
  , rewriteInfix
  ) where

import Control.Applicative hiding (some)
import Control.Arrow
import Control.Monad
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.HashMap.Strict as H
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as E

import Kitten.Abbreviation
import Kitten.Definition
import Kitten.Error
import Kitten.Fragment
import Kitten.Import
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Parse.Element
import Kitten.Parse.Layout
import Kitten.Parse.Monad
import Kitten.Parse.Primitive
import Kitten.Parse.Type
import Kitten.Parsec
import Kitten.Program
import Kitten.Term
import Kitten.Token
import Kitten.Type
import Kitten.TypeDefinition
import Kitten.Util.Either
import Kitten.Util.List
import Kitten.Util.Maybe
import Kitten.Util.Parsec

parse
  :: String
  -> [Located]
  -> Either ErrorGroup (Fragment ParsedTerm)
parse name tokens = mapLeft parseError $ do
  inserted <- Parsec.runParser insertBraces topLevelEnv name tokens
  Parsec.runParser (setInitialPosition name inserted >> fragment)
    topLevelEnv name inserted

topLevelEnv :: Env
topLevelEnv = Env { envCurrentVocabulary = Qualifier V.empty }

setInitialPosition :: String -> [Located] -> Parser ()
setInitialPosition name [] = setPosition (newPos name 1 1)
setInitialPosition _ (Located{..} : _)
  = setPosition (locationStart locatedLocation)

fragment :: Parser (Fragment ParsedTerm)
fragment = do
  loc <- getLocation
  partitionElements loc . concat
    <$> many (vocabulary <|> ((:[]) <$> element)) <* eof

vocabulary :: Parser [Element]
vocabulary = (<?> "vocabulary declaration") $ do
  void (match TkVocab)
  qualifier <- qualified_ >>= \case
    MixfixName{} -> fail "vocabulary names cannot be mixfix"
    Qualified (Qualifier parts) name -> return $ Qualifier (V.snoc parts name)
    Unqualified name -> return $ Qualifier (V.singleton name)
  choice
    [ do
      modifyState $ \env -> env { envCurrentVocabulary = qualifier }
      [] <$ match (TkOperator ";")
    , do
      original <- envCurrentVocabulary <$> getState
      modifyState $ \env -> env { envCurrentVocabulary = qualifier }
      elements <- blocked (many element)
      modifyState $ \env -> env { envCurrentVocabulary = original }
      return elements
    ]

element :: Parser Element
element = choice
  [ AbbreviationElement <$> abbreviation
  , DefElement <$> def
  , ImportElement <$> import_
  , OperatorElement <$> operatorDeclaration
  , TermElement <$> term
  , TypeElement <$> typeDef
  ]

abbreviation :: Parser Abbreviation
abbreviation = (<?> "abbreviation") $ do
  loc <- getLocation <* match TkAbbrev
  here <- envCurrentVocabulary <$> getState
  name <- named >>= \case
    Unqualified text -> return text
    _ -> fail "abbreviations must have unqualified names"
  qualifier <- qualified_ >>= \case
    Qualified (Qualifier xs) x -> return $ Qualifier (V.snoc xs x)
    Unqualified x -> return $ Qualifier (V.singleton x)
    _ -> fail "abbreviations must be short for qualifiers"
  return $ Abbreviation here name qualifier loc

def :: Parser (Def ParsedTerm)
def = (<?> "definition") $ do
  defLoc <- getLocation <* match TkDefine
  (fixity, name) <- choice
    [ (,) Postfix <$> nonsymbolic
    , (,) Infix <$> symbolic
    ] <?> "definition name"
  anno <- signature
  bodyTerm <- (<?> "definition body") $ choice [block, blockLambda]
  qualifier <- envCurrentVocabulary <$> getState
  qualifiedName <- case name of
    Qualified{} -> fail "definition names cannot be qualified"
    Unqualified text -> return (Qualified qualifier text)
    MixfixName{}
      | Qualifier xs <- qualifier, not (V.null xs)
      -> fail "mixfix definitions must be in the root vocabulary"
      | otherwise -> return name
  return Def
    { defAnno = anno
    , defFixity = fixity
    , defLocation = defLoc
    , defName = qualifiedName
    , defTerm = mono bodyTerm
    }

typeDef :: Parser TypeDef
typeDef = (<?> "type definition") $ do
  loc <- getLocation <* match TkData
  qualifier <- envCurrentVocabulary <$> getState
  name <- named <?> "type name"
  (qualifiedName, text) <- case name of
    Unqualified text -> return (Qualified qualifier text, text)
    Qualified{} -> fail "type definition names cannot be qualified"
    MixfixName{} -> fail "type definition names cannot be mixfix"
  scalars <- option mempty scalarQuantifier
  constructors <- blocked (manyV (typeConstructor qualifier text))
  return TypeDef
    { typeDefConstructors = constructors
    , typeDefLocation = loc
    , typeDefName = qualifiedName
    , typeDefScalars = V.map fst scalars  -- TODO Use locations?
    }

typeConstructor :: Qualifier -> Text -> Parser TypeConstructor
typeConstructor (Qualifier qualifier) typeName = (<?> "type constructor") $ do
  loc <- getLocation <* match TkCase
  name <- named <?> "constructor name"
  text <- case name of
    Unqualified text -> return text
    _ -> fail "type constructor names must be unqualified"
  fields <- option V.empty $ grouped (type_ `sepEndByV` match TkComma)
  return TypeConstructor
    { ctorFields = fields
    , ctorLocation = loc
    , ctorName = Qualified (Qualifier (V.snoc qualifier typeName)) text
    }

import_ :: Parser Import
import_ = (<?> "import") $ do
  loc <- getLocation <* match TkImport
  name <- qualified_
  return Import
    { importName = name
    , importLocation = loc
    }

operatorDeclaration :: Parser Operator
operatorDeclaration = (<?> "operator declaration") $ uncurry Operator
  <$> (match TkInfix *> choice
    [ ((,) NonAssociative <$> precedence)
    , match (TkWord "left") *> ((,) LeftAssociative <$> precedence)
    , match (TkWord "right") *> ((,) RightAssociative <$> precedence)
    ])
  <*> symbolic
  where
  precedence = (<?> "decimal integer precedence from 0 to 9")
    $ mapOne $ \case
      TkInt value DecimalHint
        | value >= 0 && value <= 9 -> Just value
      _ -> Nothing

term :: Parser ParsedTerm
term = do
  loc <- getLocation
  choice
    [ try $ do
      literal <- mapOne toLiteral <?> "literal"
      return $ TrPush (literal loc) loc
    , TrCall Infix <$> symbolic <*> pure loc
    , TrCall Postfix <$> (try mixfixName <|> qualified_) <*> pure loc
    , mapOne toIntrinsic <*> pure loc <?> "intrinsic"
    , try section
    , try group <?> "grouped expression"
    , pair <$> tuple <*> pure loc
    , TrMakeVector <$> vector <*> pure loc
    , lambda
    , matchCase
    , TrPush <$> blockValue <*> pure loc
    ]
  where

  section :: Parser ParsedTerm
  section = (<?> "operator section") . grouped $ choice
    [ do
      loc <- getLocation
      function <- symbolic
      choice
        [ do
          operandLoc <- getLocation
          operand <- many1 term
          let
            operandLoc' = case operand of
              x : _ -> parsedLocation x
              _ -> operandLoc
          return $ TrCompose StackAny (V.fromList
            [ TrCompose Stack1 (V.fromList operand) operandLoc'
            , TrCall Postfix function loc
            ]) loc
        , return $ TrCall Postfix function loc
        ]
    , do
      operandLoc <- getLocation
      operand <- many1 (notFollowedBy symbolic *> term)
      let
        operandLoc' = case operand of
          x : _ -> parsedLocation x
          _ -> operandLoc
      loc <- getLocation
      function <- symbolic
      return $ TrCompose StackAny (V.fromList
        [ TrCompose Stack1 (V.fromList operand) operandLoc'
        , swap loc
        , TrCall Postfix function loc
        ]) loc
    ]
    where
    swap loc = TrLambda "right operand" loc (TrLambda "left operand" loc
      (TrCompose StackAny (V.fromList
        [ TrCall Postfix "right operand" loc
        , TrCall Postfix "left operand" loc
        ]) loc) loc) loc

  matchCase :: Parser ParsedTerm
  matchCase = (<?> "match") $ do
    matchLoc <- getLocation <* match TkMatch
    scrutineeLoc <- getLocation
    mScrutinee <- optionMaybe group <?> "scrutinee"
    (patterns, mDefault) <- blocked $ do
      patterns <- manyV . (<?> "case") $ do
        caseLoc <- getLocation <* match TkCase
        name <- qualified_
        bodyLoc <- getLocation
        body <- choice [block, blockLambda]
        return $ TrCase name (TrQuotation body bodyLoc) caseLoc
      mDefault <- optionMaybe $ do
        defaultLoc <- getLocation <* match TkDefault
        TrQuotation <$> block <*> pure defaultLoc
      return (patterns, mDefault)
    let
      withScrutinee loc scrutinee x
        = TrCompose StackAny (V.fromList [scrutinee, x]) loc
    return $ maybe id (withScrutinee scrutineeLoc) mScrutinee
      $ TrMatch patterns mDefault matchLoc

  pair :: Vector ParsedTerm -> Location -> ParsedTerm
  pair values loc = V.foldr1 (\x y -> TrMakePair x y loc) values

  toIntrinsic :: Token -> Maybe (Location -> ParsedTerm)
  toIntrinsic (TkIntrinsic name) = Just $ TrIntrinsic name
  toIntrinsic _ = Nothing

  tuple :: Parser (Vector ParsedTerm)
  tuple = grouped
    (tupleElement `sepEndBy1V` match TkComma)
    <?> "tuple"
    where
    tupleElement = do
      loc <- getLocation
      TrCompose StackAny <$> many1V term <*> pure loc

vector :: Parser (Vector ParsedTerm)
vector = vectored
  (vectorElement `sepEndByV` match TkComma)
  <?> "vector"
  where
  vectorElement = do
    loc <- getLocation
    TrCompose StackAny <$> many1V term <*> pure loc

group :: Parser ParsedTerm
group = (<?> "group") $ do
  loc <- getLocation
  TrCompose StackAny <$> grouped (many1V term) <*> pure loc

lambda :: Parser ParsedTerm
lambda = (<?> "lambda") $ do
  choice
    [ try $ plainLambda
    , do
      body <- blockLambda
      let loc = parsedLocation body
      return $ TrPush (TrQuotation body loc) loc
    ]

plainLambda :: Parser ParsedTerm
plainLambda = match TkArrow *> do
  names <- lambdaNames <* match (TkOperator ";")
  bodyLoc <- getLocation
  body <- blockContents
  return $ makeLambda names body bodyLoc

lambdaNames :: Parser [(Maybe Name, Location)]
lambdaNames = many1 $ do
  loc <- getLocation
  name <- Just <$> named <|> Nothing <$ ignore
  return (name, loc)

lambdaBlock :: Parser ([(Maybe Name, Location)], ParsedTerm, Location)
lambdaBlock = match TkArrow *> do
  names <- lambdaNames
  loc <- getLocation
  body <- block
  return (names, body, loc)

blockLambda :: Parser ParsedTerm
blockLambda = do
  (names, body, loc) <- lambdaBlock
  return $ makeLambda names body loc

makeLambda :: [(Maybe Name, Location)] -> ParsedTerm -> Location -> ParsedTerm
makeLambda namesAndLocs body bodyLoc = foldr
  (\ (mLambdaName, nameLoc) lambdaTerms -> maybe
    (TrCompose StackAny (V.fromList
      [ TrLambda "_" nameLoc (TrCompose StackAny V.empty bodyLoc) bodyLoc
      , lambdaTerms
      ]) bodyLoc)
    (\lambdaName -> TrLambda lambdaName nameLoc lambdaTerms bodyLoc)
    mLambdaName)
  body
  (reverse namesAndLocs)

blockValue :: Parser ParsedValue
blockValue = (<?> "function") $ do
  loc <- getLocation
  let
    reference = TrCall Postfix
      <$> (match TkReference *> qualified_) <*> pure loc
  TrQuotation <$> (block <|> reference) <*> pure loc

toLiteral :: Token -> Maybe (Location -> ParsedValue)
toLiteral (TkBool x) = Just $ TrBool x
toLiteral (TkChar x) = Just $ TrChar x
toLiteral (TkFloat x) = Just $ TrFloat x
toLiteral (TkInt x _) = Just $ TrInt x
toLiteral (TkText x) = Just $ TrText x
toLiteral _ = Nothing

block :: Parser ParsedTerm
block = blocked blockContents <?> "block"

blockContents :: Parser ParsedTerm
blockContents = do
  loc <- getLocation
  terms <- many term
  let
    loc' = case terms of
      x : _ -> parsedLocation x
      _ -> loc
  return $ TrCompose StackAny (V.fromList terms) loc'

----------------------------------------

type TermParser a = ParsecT [ParsedTerm] Env Identity a

rewriteInfix
  :: Program
  -> Fragment ParsedTerm
  -> Either ErrorGroup (Fragment ParsedTerm, Program)
rewriteInfix program@Program{..} parsed@Fragment{..} = do
  rewrittenDefs <- T.mapM (traverse rewriteInfixTerm) fragmentDefs
  rewrittenTerm <- rewriteInfixTerm fragmentTerm
  return $ (,)
    parsed
      { fragmentDefs = rewrittenDefs
      , fragmentTerm = rewrittenTerm
      }
    program
      { programFixities = allFixities
      , programOperators = allOperators
      }

  where
  rewriteInfixTerm :: ParsedTerm -> Either ErrorGroup ParsedTerm
  rewriteInfixTerm = \case
    x@TrCall{} -> return x
    TrCompose hint terms loc -> do
      terms' <- V.toList <$> V.mapM rewriteInfixTerm terms
      let
        expression' = between (setPositionTerm loc terms') eof infixExpression
        infixExpression = TrCompose hint
          <$> manyV (expression <|> lambdaTerm)
          <*> pure loc
          <?> "infix expression"
      case Parsec.runParser expression' topLevelEnv [] terms' of
        Left message -> Left $ parseError message
        Right parseResult -> Right parseResult
    x@TrConstruct{} -> return x
    x@TrIntrinsic{} -> return x
    TrLambda name nameLoc body loc -> do
      body' <- rewriteInfixTerm body
      return $ TrLambda name nameLoc body' loc
    TrMakePair a b loc -> do
      a' <- rewriteInfixTerm a
      b' <- rewriteInfixTerm b
      return $ TrMakePair a' b' loc
    TrMakeVector terms loc -> do
      terms' <- V.mapM rewriteInfixTerm terms
      return $ TrMakeVector terms' loc
    TrMatch cases mDefault loc -> do
      cases' <- V.mapM rewriteInfixCase cases
      mDefault' <- traverse rewriteInfixValue mDefault
      return $ TrMatch cases' mDefault' loc
      where
      rewriteInfixCase (TrCase name body loc') = do
        body' <- rewriteInfixValue body
        return $ TrCase name body' loc'
    TrPush value loc -> do
      value' <- rewriteInfixValue value
      return $ TrPush value' loc

  lambdaTerm :: TermParser ParsedTerm
  lambdaTerm = satisfyTerm $ \case
    TrLambda{} -> True
    _ -> False

  mixfix :: TermParser ParsedTerm
  mixfix = do
    loc <- getPosition
    (operands, name) <- choice $ map (try . fromParts) mixfixTable
    return . mixfixCall operands name
      $ Location { locationStart = loc, locationIndent = -1 }

    where
    mixfixCall :: [ParsedTerm] -> Name -> Location -> ParsedTerm
    mixfixCall operands name loc = TrCompose StackAny
      (V.fromList operands
        <> V.singleton (TrCall Postfix {- TODO Mixfix? -} name loc))
      loc

    fromParts :: [MixfixNamePart] -> TermParser ([ParsedTerm], Name)
    fromParts = fmap (second MixfixName) . go
      where
      go (hole@MixfixHole : parts) = do
        operand <- satisfyTerm $ \case
          TrCall{} -> False
          _ -> True
        (operands, name) <- go parts
        return (operand : operands, hole : name)
      go (namePart@(MixfixNamePart part) : parts) = do
        void . satisfyTerm $ \case
          TrCall _ (Unqualified name) _ | name == part -> True
          _ -> False
        (operands, name) <- go parts
        return (operands, namePart : name)
      go [] = return ([], [])

  mixfixTable :: [[MixfixNamePart]]
  mixfixTable
    = sortBy (flip (comparing length)) . mapMaybe toMixfixParts
      $ H.keys fragmentDefs ++ H.keys programSymbols

  toMixfixParts :: Name -> Maybe [MixfixNamePart]
  toMixfixParts (MixfixName parts) = Just parts
  toMixfixParts _ = Nothing

  rewriteInfixValue :: ParsedValue -> Either ErrorGroup ParsedValue
  rewriteInfixValue = \case
    TrQuotation body loc -> TrQuotation <$> rewriteInfixTerm body <*> pure loc
    -- TODO Exhaustivity/safety.
    other -> return other

  stack1 :: Location -> ParsedTerm -> ParsedTerm
  stack1 loc x = TrCompose Stack1 (V.singleton x) loc

  binary :: Name -> Location -> ParsedTerm -> ParsedTerm -> ParsedTerm
  binary name loc x y = TrCompose StackAny (V.fromList
    [ stack1 (parsedLocation x) x
    , stack1 (parsedLocation y) y
    , TrCall Postfix name loc
    ]) loc

  binaryOp :: Name -> TermParser (ParsedTerm -> ParsedTerm -> ParsedTerm)
  binaryOp name = mapTerm $ \t -> case t of
    TrCall Infix name' loc
      | name == name' -> Just (binary name loc)
    _ -> Nothing

  expression :: TermParser ParsedTerm
  expression = E.buildExpressionParser opTable (operands <?> "operand")
    where
    operands = do
      loc <- getLocation
      results <- many1 operand
      let
        loc' = case results of
          x : _ -> parsedLocation x
          _ -> loc
      return $ TrCompose StackAny (V.fromList results) loc'
    operand = choice
      [ try mixfix
      , satisfyTerm $ \case
        TrCall Infix _ _ -> False  -- An operator is not an operand.
        TrLambda{} -> False  -- Nor is a bare lambda.
        _ -> True
      ]

  -- TODO Detect/report duplicates.
  opTable = map (map toOp) rawTable
  allOperators = fragmentOperators ++ programOperators
  allFixities = H.map defFixity fragmentDefs <> programFixities

  rawTable :: [[Operator]]
  rawTable = let
    -- TODO Make smarter than a linear search.
    useDefault name fixity = fixity == Infix
      && not (any ((name ==) . operatorName) allOperators)
    flat = allOperators
      ++ map (Operator LeftAssociative 6)
        (H.keys $ H.filterWithKey useDefault allFixities)
    in for [9,8..0] $ \p -> filter ((p ==) . operatorPrecedence) flat

  toOp :: Operator -> E.Operator [ParsedTerm] Env Identity ParsedTerm
  toOp (Operator associativity _ name) = case associativity of
    NonAssociative -> E.Infix (binaryOp name) E.AssocNone
    LeftAssociative -> E.Infix (binaryOp name) E.AssocLeft
    RightAssociative -> E.Infix (binaryOp name) E.AssocRight

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
