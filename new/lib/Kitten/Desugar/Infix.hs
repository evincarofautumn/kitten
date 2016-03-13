module Kitten.Desugar.Infix
  ( desugar
  ) where

import Control.Applicative
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Kitten.Definition (Definition)
import Kitten.Dictionary (Dictionary)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), Qualified)
import Kitten.Operator (Operator)
import Kitten.Origin (Origin)
import Kitten.Parser (getOrigin)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Text.Parsec ((<?>), ParsecT, SourcePos)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

type Rewriter a = ParsecT [Term ()] () Identity a

desugar :: Dictionary -> Definition () -> K (Definition ())
desugar dictionary definition = do
  desugared <- desugarTerms' $ Definition.body definition
  return definition { Definition.body = desugared }
  where
  desugarTerms :: [Term ()] -> K (Term ())
  desugarTerms terms = do
    terms' <- mapM desugarTerm terms
    let
      expression' = infixExpression <* Parsec.eof
      infixExpression = do
        desugaredTerms <- many $ expression <|> lambda
        let
          origin = case desugaredTerms of
            term : _ -> Term.origin term
            _ -> Definition.origin definition
        return $ Term.compose () origin desugaredTerms
    case Parsec.runParser expression' () "" terms' of
      Left parseError -> do
        report $ Report.parseError parseError
        let
          origin = case terms of
            term : _ -> Term.origin term
            _ -> Definition.origin definition
        return $ Term.compose () origin terms
      Right result -> return result

  desugarTerm :: Term () -> K (Term ())
  desugarTerm term = case term of
    Call{} -> return term
    Compose _ a b -> desugarTerms (Term.decompose a ++ Term.decompose b)
    Drop{} -> return term
    Generic{} -> error
      "generic expression should not appear before infix desugaring"
    Group a -> desugarTerms' a
    Identity{} -> return term
    If _ a b origin -> If ()
      <$> desugarTerms' a <*> desugarTerms' b <*> pure origin
    Lambda _ name _ body origin -> Lambda () name ()
      <$> desugarTerms' body <*> pure origin
    Match _ cases mElse origin -> Match ()
      <$> mapM desugarCase cases <*> traverse desugarElse mElse <*> pure origin
      where

      desugarCase :: Case () -> K (Case ())
      desugarCase (Case name body caseOrigin)
        = Case name <$> desugarTerms' body <*> pure caseOrigin

      desugarElse :: Else () -> K (Else ())
      desugarElse (Else body elseOrigin)
        = Else <$> desugarTerms' body <*> pure elseOrigin

    New{} -> return term
    NewClosure{} -> return term
    NewVector{} -> return term
    Push _ value origin -> Push () <$> desugarValue value <*> pure origin
    Swap{} -> return term
    With{} -> return term
    Word{} -> return term

  desugarValue :: Value () -> K (Value ())
  desugarValue value = case value of
    Character{} -> return value
    Closed{} -> error "closed name should not appear before infix desugaring"
    Closure names body -> Closure names <$> desugarTerms' body
    Float{} -> return value
    Integer{} -> return value
    Local{} -> error "local name should not appear before infix desugaring"
    Name{} -> return value
    Quotation body -> Quotation <$> desugarTerms' body
    Text{} -> return value

  desugarTerms' :: Term () -> K (Term ())
  desugarTerms' = desugarTerms . Term.decompose

  expression :: Rewriter (Term ())
  expression = Expr.buildExpressionParser operatorTable operand
    where
    operand = (<?> "operand") $ do
      origin <- getOrigin
      results <- Parsec.many1 $ termSatisfy $ \ term -> case term of
        Word _ Operator.Infix _ _ _ -> False
        Lambda{} -> False
        _ -> True
      return $ Term.compose () origin results

  lambda :: Rewriter (Term ())
  lambda = termSatisfy $ \ term -> case term of
    Lambda{} -> True
    _ -> False

  operatorTable :: [[Expr.Operator [Term ()] () Identity (Term ())]]
  operatorTable = map (map toOperator) rawOperatorTable

  operatorMetadata :: HashMap Qualified Operator
  operatorMetadata = Dictionary.operatorMetadata dictionary

  rawOperatorTable :: [[Operator]]
  rawOperatorTable = map
    (\ p -> HashMap.elems
      $ HashMap.filter ((== p) . Operator.precedence) operatorMetadata)
    $ reverse [minBound .. maxBound]

  toOperator :: Operator -> Expr.Operator [Term ()] () Identity (Term ())
  toOperator operator = Expr.Infix
    (binaryOperator (QualifiedName (Operator.name operator)))
    $ case Operator.associativity operator of
      Operator.Nonassociative -> Expr.AssocNone
      Operator.Leftward -> Expr.AssocRight
      Operator.Rightward -> Expr.AssocLeft

  binaryOperator :: GeneralName -> Rewriter (Term () -> Term () -> Term ())
  binaryOperator name = mapTerm $ \ term -> case term of
    Word _ Operator.Infix name' _ origin
      | name == name' -> Just $ binary name origin
    _ -> Nothing

  binary :: GeneralName -> Origin -> Term () -> Term () -> Term ()
  binary name origin x y = Term.compose () origin
    [x, y, Word () Operator.Postfix name [] origin]

  mapTerm :: (Term () -> Maybe a) -> Rewriter a
  mapTerm = Parsec.tokenPrim show advanceTerm

  termSatisfy :: (Term () -> Bool) -> Rewriter (Term ())
  termSatisfy predicate = Parsec.tokenPrim show advanceTerm
    (\ token -> if predicate token then Just token else Nothing)

  advanceTerm :: SourcePos -> t -> [Term a] -> SourcePos
  advanceTerm _ _ (term : _) = Origin.begin $ Term.origin term
  advanceTerm sourcePos _ _ = sourcePos
