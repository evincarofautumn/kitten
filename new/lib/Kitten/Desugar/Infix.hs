module Kitten.Desugar.Infix
  ( desugar
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Kitten.Definition (Definition)
import Kitten.Fragment (Fragment)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), Qualified)
import Kitten.Operator (Operator(Operator))
import Kitten.Origin (Origin)
import Kitten.Parser (getOrigin)
import Kitten.Report (reportParseError)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Text.Parsec ((<?>), ParsecT, SourcePos)
import qualified Data.Map as Map
import qualified Kitten.Definition as Definition
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Term as Term
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

type Rewriter a = ParsecT [Term] () Identity a

desugar :: Fragment -> K Fragment
desugar fragment = do
  desugared <- mapM desugarDefinition (Fragment.definitions fragment)
  return fragment { Fragment.definitions = desugared }
  where

  desugarDefinition :: Definition -> K Definition
  desugarDefinition definition = do
    desugared <- desugarTerms' $ Definition.body definition
    return definition { Definition.body = desugared }
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
              term : _ -> Term.origin term
              _ -> Definition.origin definition
          return $ Term.compose origin desugaredTerms
      case Parsec.runParser expression' () "" terms' of
        Left parseError -> do
          report $ reportParseError parseError
          let
            origin = case terms of
              term : _ -> Term.origin term
              _ -> Definition.origin definition
          return $ Term.compose origin terms
        Right result -> return result

    desugarTerm :: Term -> K Term
    desugarTerm term = case term of
      Call{} -> return term
      Compose _ a b -> desugarTerms (Term.decompose a ++ Term.decompose b)
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
    desugarTerms' = desugarTerms . Term.decompose

  expression :: Rewriter Term
  expression = Expr.buildExpressionParser operatorTable operand
    where
    operand = (<?> "operand") $ do
      origin <- getOrigin
      results <- Parsec.many1 $ termSatisfy $ \ term -> case term of
        Call _ Operator.Infix _ _ _ -> False
        Lambda{} -> False
        _ -> True
      return $ Term.compose origin results

  lambda :: Rewriter Term
  lambda = termSatisfy $ \ term -> case term of
    Lambda{} -> True
    _ -> False

  operatorTable :: [[Expr.Operator [Term] () Identity Term]]
  operatorTable = map (map toOperator) rawOperatorTable

  fragmentFixities :: Map Qualified Operator.Fixity
  fragmentFixities = Map.fromList
    $ map (Definition.name &&& Definition.fixity)
    $ Fragment.definitions fragment

  rawOperatorTable :: [[Operator]]
  rawOperatorTable = map
    (\ p -> filter ((p ==) . Operator.precedence) flat)
    $ reverse [minBound .. maxBound]
    where

    useDefault :: Qualified -> Operator.Fixity -> Bool
    useDefault name fixity = fixity == Operator.Infix
      && not (any ((name ==) . Operator.name) $ Fragment.operators fragment)

    flat :: [Operator]
    flat = concat
      [ Fragment.operators fragment
      , map makeDefaultOperator
        $ Map.keys $ Map.filterWithKey useDefault fragmentFixities
      ]
      where

      makeDefaultOperator :: Qualified -> Operator
      makeDefaultOperator name = Operator
        { Operator.associativity = Operator.Leftward
        , Operator.name = name
        , Operator.precedence = Operator.Precedence 6
        }

  toOperator :: Operator -> Expr.Operator [Term] () Identity Term
  toOperator operator = Expr.Infix
    (binaryOperator (QualifiedName (Operator.name operator)))
    $ case Operator.associativity operator of
      Operator.Nonassociative -> Expr.AssocNone
      Operator.Leftward -> Expr.AssocRight
      Operator.Rightward -> Expr.AssocLeft

  binaryOperator :: GeneralName -> Rewriter (Term -> Term -> Term)
  binaryOperator name = mapTerm $ \ term -> case term of
    Call _ Operator.Infix name' _ origin
      | name == name' -> Just $ binary name origin
    _ -> Nothing

  binary :: GeneralName -> Origin -> Term -> Term -> Term
  binary name origin x y = Term.compose origin
    [x, y, Call Nothing Operator.Postfix name [] origin]

  mapTerm :: (Term -> Maybe a) -> Rewriter a
  mapTerm = Parsec.tokenPrim show advanceTerm

  termSatisfy :: (Term -> Bool) -> Rewriter Term
  termSatisfy predicate = Parsec.tokenPrim show advanceTerm
    (\ token -> if predicate token then Just token else Nothing)

  advanceTerm :: SourcePos -> t -> [Term] -> SourcePos
  advanceTerm _ _ (term : _) = Origin.begin $ Term.origin term
  advanceTerm sourcePos _ _ = sourcePos
