{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Traversable (traverse)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.HashMap.Strict as H
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as E

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
  inserted <- Parsec.parse insertBraces name tokens
  Parsec.parse (setInitialPosition name inserted >> fragment) name inserted

setInitialPosition :: String -> [Located] -> Parser ()
setInitialPosition name [] = setPosition (newPos name 1 1)
setInitialPosition _ (Located{..} : _)
  = setPosition (locationStart locatedLocation)

fragment :: Parser (Fragment ParsedTerm)
fragment = do
  loc <- getLocation
  fmap (partitionElements loc . concat)
    $ many element `sepEndBy` match TkSemicolon <* eof

element :: Parser Element
element = choice
  [ DefElement <$> def 
  , ImportElement <$> import_
  , OperatorElement <$> operatorDeclaration
  , TermElement <$> term
  , TypeElement <$> typeDef
  ]

def :: Parser (Def ParsedTerm)
def = (<?> "definition") . locate $ do
  void (match TkDefine)
  (fixity, name) <- choice
    [ (,) Postfix <$> nonsymbolic
    , (,) Infix <$> symbolic
    ] <?> "definition name"
  anno <- signature
  bodyTerm <- (<?> "definition body") . locate $ choice
    [ TrCompose StackAny <$> block
    , do
      (names, body) <- lambdaBlock
      return $ makeLambda names body
    ]
  return $ \loc -> Def
    { defAnno = anno
    , defFixity = fixity
    , defLocation = loc
    , defName = name
    , defTerm = mono bodyTerm
    }

typeDef :: Parser TypeDef
typeDef = (<?> "type definition") . locate $ do
  void (match TkData)
  name <- named <?> "type name"
  scalars <- option mempty scalarQuantifier
  constructors <- blocked (manyV typeConstructor)
  return $ \loc -> TypeDef
    { typeDefConstructors = constructors
    , typeDefLocation = loc
    , typeDefName = name
    , typeDefScalars = scalars
    }

typeConstructor :: Parser TypeConstructor
typeConstructor = (<?> "type constructor") . locate $ do
  void (match TkCase)
  name <- named <?> "constructor name"
  fields <- manyV baseType <* match TkSemicolon
  return $ \loc -> TypeConstructor
    { ctorFields = fields
    , ctorLocation = loc
    , ctorName = name
    }

import_ :: Parser Import
import_ = (<?> "import") . locate $ do
  void (match TkImport)
  name <- qualified_
  return $ \loc -> Import
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
term = locate $ choice
  [ try $ TrPush <$> locate (mapOne toLiteral <?> "literal")
  , TrCall Infix <$> symbolic
  , TrCall Postfix <$> (try mixfixName <|> qualified_)
  , mapOne toIntrinsic <?> "intrinsic"
  , try section
  , try group <?> "grouped expression"
  , pair <$> tuple
  , TrMakeVector <$> vector
  , lambda
  , matchCase
  , TrPush <$> blockValue
  ]
  where

  section :: Parser (Location -> ParsedTerm)
  section = (<?> "operator section") $ grouped $ choice
    [ do
      function <- symbolic
      choice
        [ do
          operand <- many1V term
          return $ \loc -> TrCompose StackAny (V.fromList
            [ TrCompose Stack1 operand loc
            , TrCall Postfix function loc
            ]) loc
        , return $ TrCall Postfix function
        ]
    , do
      operand <- many1V (notFollowedBy symbolic *> term)
      function <- symbolic
      return $ \loc -> TrCompose StackAny (V.fromList
        [ TrCompose Stack1 operand loc
        , swap loc
        , TrCall Postfix function loc
        ]) loc
    ]
    where
    swap loc = TrLambda "right operand" (TrLambda "left operand"
      (TrCompose StackAny (V.fromList
        [ TrCall Postfix "right operand" loc
        , TrCall Postfix "left operand" loc
        ]) loc) loc) loc

  matchCase :: Parser (Location -> ParsedTerm)
  matchCase = (<?> "match") $ match TkMatch *> do
    mScrutinee <- optionMaybe group <?> "scrutinee"
    (patterns, mDefault) <- blocked $ do
      patterns <- manyV . locate $ match TkCase *> do
        name <- named
        body <- choice
          [ do
            body <- block
            return $ const body
          , do
            (names, body) <- lambdaBlock
            return $ \loc -> V.singleton (makeLambda names body loc)
          ]
        return $ \loc -> TrCase name
          (TrQuotation (TrCompose StackAny (body loc) loc) loc) loc
      mDefault <- optionMaybe . locate $ match TkDefault *> do
        body <- block
        return $ \loc -> TrQuotation (TrCompose StackAny body loc) loc
      return (patterns, mDefault)
    let
      withScrutinee loc scrutinee x
        = TrCompose StackAny (V.fromList [scrutinee loc, x]) loc
    return $ \loc -> maybe id (withScrutinee loc) mScrutinee
      $ TrMatch patterns mDefault loc

  pair :: Vector ParsedTerm -> Location -> ParsedTerm
  pair values loc = V.foldr1 (\x y -> TrMakePair x y loc) values

  toIntrinsic :: Token -> Maybe (Location -> ParsedTerm)
  toIntrinsic (TkIntrinsic name) = Just $ TrIntrinsic name
  toIntrinsic _ = Nothing

  tuple :: Parser (Vector ParsedTerm)
  tuple = grouped
    (locate (TrCompose StackAny <$> many1V term)
      `sepEndBy1V` match TkComma)
    <?> "tuple"

vector :: Parser (Vector ParsedTerm)
vector = vectored
  (locate (TrCompose StackAny <$> many1V term)
    `sepEndByV` match TkComma)
  <?> "vector"

group :: Parser (Location -> ParsedTerm)
group = (<?> "group") $ TrCompose StackAny <$> grouped (many1V term)

lambda :: Parser (Location -> ParsedTerm)
lambda = (<?> "lambda") $ choice
  [ try plainLambda
  , do
    (names, body) <- lambdaBlock
    return $ \loc -> TrPush (TrQuotation (makeLambda names body loc) loc) loc
  ]

plainLambda :: Parser (Location -> ParsedTerm)
plainLambda = match TkArrow *> do
  names <- lambdaNames <* match TkSemicolon
  body <- blockContents
  return $ \loc -> makeLambda names body loc

lambdaNames :: Parser [Maybe Name]
lambdaNames = many1 $ Just <$> named <|> Nothing <$ underscore

lambdaBlock :: Parser ([Maybe Name], Vector ParsedTerm)
lambdaBlock = match TkArrow *> do
  names <- lambdaNames
  body <- block
  return (names, body)

makeLambda :: [Maybe Name] -> Vector ParsedTerm -> Location -> ParsedTerm
makeLambda names terms loc = foldr
  (\mLambdaName lambdaTerms -> maybe
    (TrCompose StackAny (V.fromList
      [ TrLambda "_" (TrCompose StackAny V.empty loc) loc
      , lambdaTerms
      ]) loc)
    (\lambdaName -> TrLambda lambdaName lambdaTerms loc)
    mLambdaName)
  (TrCompose StackAny terms loc)
  (reverse names)

blockValue :: Parser ParsedValue
blockValue = (<?> "function") . locate $ do
  terms <- block <|> V.singleton <$> reference
  return $ \loc -> TrQuotation (TrCompose StackAny terms loc) loc
  where
  reference = locate $ TrCall Postfix <$> (match TkReference *> word)

toLiteral :: Token -> Maybe (Location -> ParsedValue)
toLiteral (TkBool x) = Just $ TrBool x
toLiteral (TkChar x) = Just $ TrChar x
toLiteral (TkFloat x) = Just $ TrFloat x
toLiteral (TkInt x _) = Just $ TrInt x
toLiteral (TkText x) = Just $ TrText x
toLiteral _ = Nothing

block :: Parser (Vector ParsedTerm)
block = blocked blockContents <?> "block"

blockContents :: Parser (Vector ParsedTerm)
blockContents = locate $ do
  void $ optional semi
  groups <- many1V term `sepEndBy` semi
  return $ \loc -> V.fromList $ map (\t -> TrCompose StackAny t loc) groups
  where semi = match TkSemicolon

----------------------------------------

type TermParser a = ParsecT [ParsedTerm] () Identity a

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
      case Parsec.parse expression' [] terms' of
        Left message -> Left $ parseError message
        Right parseResult -> Right parseResult
    x@TrConstruct{} -> return x
    x@TrIntrinsic{} -> return x
    TrLambda name body loc -> do
      body' <- rewriteInfixTerm body
      return $ TrLambda name body' loc
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
    [ stack1 loc x
    , stack1 loc y
    , TrCall Postfix name loc
    ]) loc

  binaryOp :: Name -> TermParser (ParsedTerm -> ParsedTerm -> ParsedTerm)
  binaryOp name = mapTerm $ \t -> case t of
    TrCall Infix name' loc
      | name == name' -> Just (binary name loc)
    _ -> Nothing

  expression :: TermParser ParsedTerm
  expression = E.buildExpressionParser opTable
    (locate (TrCompose StackAny <$> many1V operand) <?> "operand")
    where
    operand :: TermParser ParsedTerm
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

  toOp :: Operator -> E.Operator [ParsedTerm] () Identity ParsedTerm
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
