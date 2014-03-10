{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -w #-}

module Kitten.Parse
  ( parse
  , rewriteInfix
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Function
import Data.Functor.Identity
import Data.List (nubBy)
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
import Kitten.Type (mono)
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
fragment = fmap partitionElements $ many element <* eof

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
    [ (,) PostfixHint <$> littleWord
    , (,) InfixHint <$> operator
    ] <?> "definition name"
  anno <- signature
  bodyTerm <- locate (Compose <$> block) <?> "definition body"
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
  name <- bigWord
  return $ \loc -> Import
    { importName = name
    , importLocation = loc
    }

operatorDeclaration :: Parser Operator
operatorDeclaration = (<?> "operator declaration") . locate $ Operator
  <$> choice
    [ Infix <$ match Token.Infix
    , InfixLeft <$ match Token.InfixLeft
    , InfixRight <$ match Token.InfixRight
    , Postfix <$ match Token.Postfix
    , Prefix <$ match Token.Prefix
    ]
  <*> mapOne (\case Token.Int value _ -> Just value; _ -> Nothing)
  <*> operator

term :: Parser ParsedTerm
term = nonblockTerm <|> blockTerm
  where
  nonblockTerm :: Parser ParsedTerm
  nonblockTerm = locate $ choice
    [ try $ Push <$> nonblockValue
    , Call PostfixHint <$> littleWord
    , Call InfixHint <$> operator
    , try $ Call PostfixHint
      <$> (grouped operator <?> "parenthesized operator")
    , try group <?> "grouped expression"
    , pair <$> tuple
    , VectorTerm <$> vector
    , mapOne toBuiltin <?> "builtin"
    , lambda
    , doElse
    ]

  blockTerm :: Parser ParsedTerm
  blockTerm = locate $ Push <$> blockValue

  group :: Parser (Location -> ParsedTerm)
  group = (<?> "group") $ UnparsedApplicative <$> grouped (many1 term)

  doElse :: Parser (Location -> ParsedTerm)
  doElse = (<?> "do block") $ match Token.Do *> doBody

  toFunctionOrBuiltin :: Token -> Maybe Text
  toFunctionOrBuiltin token = case token of
    Token.Builtin name -> Builtin.toText name
    Token.LittleWord name -> Just name
    Token.Operator name -> Just name
    _ -> Nothing

  doBody :: Parser (Location -> ParsedTerm)
  doBody = (<?> "do body") $ do
    name <- mapOne toFunctionOrBuiltin
    open <- many nonblockTerm
    body <- blockTerm
    else_ <- optionMaybe $ match Token.Else *> choice
      [ try . locate $ Push
        <$> locate (Function <$> locate doBody)
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
        [ V.fromList open
        , V.singleton body
        , else'
        , V.singleton (name'' loc)
        ]
      in Compose whole loc

  lambda :: Parser (Location -> ParsedTerm)
  lambda = (<?> "lambda") $ match Token.Arrow *> choice
    [ Lambda <$> littleWord <*> locate (Compose <$> manyV term)
    , do
      names <- blocked (many littleWord)
      terms <- manyV term
      return $ \loc -> foldr
        (\lambdaName lambdaTerms -> Lambda lambdaName lambdaTerms loc)
        (Compose terms loc)
        (reverse names)
    ]

  pair :: Vector ParsedTerm -> Location -> ParsedTerm
  pair values loc = V.foldr1 (\x y -> PairTerm x y loc) values

  toBuiltin :: Token -> Maybe (Location -> ParsedTerm)
  toBuiltin (Token.Builtin name) = Just $ Builtin name
  toBuiltin _ = Nothing

  tuple :: Parser (Vector ParsedTerm)
  tuple = grouped
    (locate (Compose <$> many1V term) `sepEndBy1V` match Token.Comma)
    <?> "tuple"

  vector :: Parser (Vector ParsedTerm)
  vector = between
    (match Token.VectorBegin)
    (match Token.VectorEnd)
    (locate (Compose <$> many1V term) `sepEndByV` match Token.Comma)
    <?> "vector"

blockValue :: Parser ParsedValue
blockValue = (<?> "function") . locate $ do
  terms <- block
  return $ \loc -> Function (Compose terms loc) loc

nonblockValue :: Parser ParsedValue
nonblockValue = choice
  [ locate (mapOne toLiteral <?> "literal")
  , unit
  ]

toLiteral :: Token -> Maybe (Location -> ParsedValue)
toLiteral (Token.Bool x) = Just $ Bool x
toLiteral (Token.Char x) = Just $ Char x
toLiteral (Token.Float x) = Just $ Float x
toLiteral (Token.Int x _) = Just $ Int x
toLiteral (Token.Text x) = Just $ String x
toLiteral _ = Nothing

unit :: Parser ParsedValue
unit = locate $ Unit <$ (match Token.GroupBegin >> match Token.GroupEnd)

block :: Parser (Vector ParsedTerm)
block = blocked (manyV term) <?> "block"

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
    Compose terms loc -> do
      terms' <- V.mapM rewriteInfixTerm terms
      return $ Compose terms' loc
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
    UnparsedApplicative terms _loc -> do
      terms' <- mapM rewriteInfixTerm terms
      case Parsec.parse (expression <* eof) [] terms' of
        Left message -> Left $ parseError message
        Right parseResult -> Right parseResult
    VectorTerm terms loc -> do
      terms' <- V.mapM rewriteInfixTerm terms
      return $ VectorTerm terms' loc

  rewriteInfixValue :: ParsedValue -> Either ErrorGroup ParsedValue
  rewriteInfixValue = \case
    Function body loc -> Function <$> rewriteInfixTerm body <*> pure loc
    -- TODO Exhaustivity/safety.
    other -> return other

  binary :: Text -> Location -> ParsedTerm -> ParsedTerm -> ParsedTerm
  binary name loc x y = Compose
    (V.fromList [x, y, Call InfixHint name loc]) loc

  unary :: Text -> Location -> ParsedTerm -> ParsedTerm
  unary name loc x = Compose
    (V.fromList [x, Call InfixHint name loc]) loc

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
    (locate $ Compose <$> many1V nonOp)
    where
    nonOp :: TermParser ParsedTerm
    nonOp = satisfyTerm $ \case Call InfixHint _ _ -> False; _ -> True

  -- TODO Detect/report duplicates.
  opTable = map (map toOp) rawTable

  rawTable :: [[Operator]]
  rawTable = let
    -- TODO Make smarter than a linear search.
    useDefault def = defFixityHint def == InfixHint
      && not (any ((defName def ==) . operatorName) fragmentOperators)
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
