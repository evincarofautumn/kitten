{-|
Module      : Kitten.Desugar.Infix
Description : Desugaring infix operators to postfix
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kitten.Desugar.Infix
  ( desugar
  ) where

import Control.Applicative
import Data.Functor.Identity (Identity)
import Kitten.Definition (Definition)
import Kitten.Dictionary (Dictionary)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..))
import Kitten.Operator (Operator)
import Kitten.Origin (Origin, getOrigin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..), composed, decomposed)
import Text.Parsec ((<?>), ParsecT, SourcePos)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

type Rewriter a = ParsecT [Sweet 'Postfix] () Identity a

-- | Desugars infix operators into postfix calls in the body of a 'Definition',
-- according to the definitions and operator metadata in the 'Dictionary'.

desugar :: Dictionary -> Definition 'Resolved -> K (Definition 'Postfix)
desugar dictionary definition = do
  operatorMetadata <- Dictionary.operatorMetadata dictionary
  let
    operatorTable
      :: [[Expr.Operator [Sweet 'Postfix] () Identity (Sweet 'Postfix)]]
    operatorTable = map (map toOperator) rawOperatorTable

    rawOperatorTable :: [[Operator]]
    rawOperatorTable = map
      (\ p -> HashMap.elems
        $ HashMap.filter ((== p) . Operator.precedence) operatorMetadata)
      $ reverse [minBound .. maxBound]

    expression :: Rewriter (Sweet 'Postfix)
    expression = Expr.buildExpressionParser operatorTable operand
      where
      operand = (<?> "operand") $ do
        origin <- getTermOrigin
        results <- Parsec.many1 $ termSatisfy $ \ term -> case term of
          SWord _ _ Operator.Infix _ _ -> False
          SLambda{} -> False
          _ -> True
        return $ composed origin results

    desugarTerms :: [Sweet 'Resolved] -> K (Sweet 'Postfix)
    desugarTerms terms = do
      terms' <- mapM desugarTerm terms
      let
        expression' = infixExpression <* Parsec.eof
        infixExpression = do
          desugaredTerms <- many $ expression <|> lambda
          let
            origin = case desugaredTerms of
              term : _ -> getOrigin term
              _ -> Definition.origin definition
          return $ composed origin desugaredTerms
      case Parsec.runParser expression' () "" terms' of
        Left parseError -> do
          report $ Report.parseError parseError
          let
            origin = case terms of
              term : _ -> getOrigin term
              _ -> Definition.origin definition
          return $ composed origin terms'
        Right result -> return result

    desugarTerm :: Sweet 'Resolved -> K (Sweet 'Postfix)
    desugarTerm term = case term of
      SArray _ origin items -> SArray () origin <$> mapM desugarTerms' items
      SAs _ origin types -> pure $ SAs () origin types
      SCharacter _ origin text -> pure $ SCharacter () origin text
      SCompose _ a b -> desugarTerms $ decomposed a ++ decomposed b
      SDo _ origin a b -> SDo () origin <$> desugarTerms' a <*> desugarTerms' b
      SEscape _ origin body -> SEscape () origin <$> desugarTerms' body
      SFloat _ origin literal -> pure $ SFloat () origin literal
      SGeneric{} -> error "generic term shouldn't appear before infix desugaring"
      -- FIXME: SGroup shouldn't need to exist after infix desugaring, but with
      -- the way things are currently organized, it needs to be present *during*
      -- the desugaring, so we have a case for it here.
      SGroup _ origin body -> SGroup () origin <$> desugarTerms' body
      SIdentity _ origin -> pure $ SIdentity () origin
      SIf _ origin mCondition true elifs mElse -> SIf () origin
        <$> traverse desugarTerms' mCondition
        <*> desugarTerms' true
        <*> traverse
          (\ (elifOrigin, condition, body) -> (,,) elifOrigin
            <$> desugarTerms' condition
            <*> desugarTerms' body) elifs
        <*> traverse desugarTerms' mElse

      -- GHC warns about this missing case, but it is an error to include it. :(
      -- SInfix{}

      SInteger _ origin literal -> pure $ SInteger () origin literal

      SJump _ origin -> pure $ SJump () origin

      SLambda _ origin names body
        -> SLambda () origin names <$> desugarTerms' body

      SList _ origin items -> SList () origin <$> mapM desugarTerms' items

      SLocal _ origin name -> pure $ SLocal () origin name

      SLoop _ origin -> pure $ SLoop () origin

      SMatch _ origin mScrutinee cases mElse -> SMatch () origin
        <$> traverse desugarTerms' mScrutinee
        <*> traverse
          (\ (caseOrigin, name, body)
             -> (,,) caseOrigin name <$> desugarTerms' body)
          cases
        <*> traverse desugarTerms' mElse

      SNestableCharacter _ origin text -> pure $ SNestableCharacter () origin text

      SNestableText _ origin text -> pure $ SNestableText () origin text

      SParagraph _ origin text -> pure $ SParagraph () origin text

      STag _ origin size index -> pure $ STag () origin size index

      SText _ origin text -> pure $ SText () origin text

      SQuotation _ origin body -> SQuotation () origin <$> desugarTerms' body

      SReturn _ origin -> pure $ SReturn () origin

      SSection _ origin name swap operand
        -> SSection () origin name swap <$> desugarTerms' operand

      STodo _ origin -> pure $ STodo () origin

      SUnboxedQuotation _ origin body
        -> SUnboxedQuotation () origin <$> desugarTerms' body

      SWith _ origin permits -> pure $ SWith () origin permits

      SWord _ origin fixity name args
        -> pure $ SWord () origin fixity name args

    desugarTerms' :: Sweet 'Resolved -> K (Sweet 'Postfix)
    desugarTerms' = desugarTerms . decomposed

  desugared <- desugarTerms' $ Definition.body definition
  return definition { Definition.body = desugared }
  where

  lambda :: Rewriter (Sweet 'Postfix)
  lambda = termSatisfy $ \ term -> case term of
    SLambda{} -> True
    _ -> False

  toOperator
    :: Operator
    -> Expr.Operator [Sweet 'Postfix] () Identity (Sweet 'Postfix)
  toOperator operator = Expr.Infix
    (binaryOperator (QualifiedName (Operator.name operator)))
    $ case Operator.associativity operator of
      Operator.Nonassociative -> Expr.AssocNone
      Operator.Leftward -> Expr.AssocRight
      Operator.Rightward -> Expr.AssocLeft

  binaryOperator
    :: GeneralName
    -> Rewriter (Sweet 'Postfix -> Sweet 'Postfix -> Sweet 'Postfix)
  binaryOperator name = mapTerm $ \ term -> case term of
    SWord _ origin Operator.Infix name' _
      | name == name' -> Just $ binary name origin
    _ -> Nothing

  binary
    :: GeneralName
    -> Origin
    -> Sweet 'Postfix
    -> Sweet 'Postfix
    -> Sweet 'Postfix
  binary name origin x y = SInfix () origin x name y

  mapTerm :: (Sweet 'Postfix -> Maybe a) -> Rewriter a
  mapTerm = Parsec.tokenPrim show advanceTerm

  termSatisfy :: (Sweet 'Postfix -> Bool) -> Rewriter (Sweet 'Postfix)
  termSatisfy predicate = Parsec.tokenPrim show advanceTerm
    (\ token -> if predicate token then Just token else Nothing)

  advanceTerm :: SourcePos -> t -> [Sweet p] -> SourcePos
  advanceTerm _ _ (term : _) = Origin.begin $ getOrigin term
  advanceTerm sourcePos _ _ = sourcePos

  getTermOrigin = getOrigin
    <$> Parsec.lookAhead (termSatisfy (const True))
