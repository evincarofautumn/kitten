{-# LANGUAGE OverloadedStrings #-}

module Kitten.Resolve
  ( resolveNames
  ) where

import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.List (elemIndex)
import Data.Set (Set)
import GHC.Exts (groupWith)
import Kitten.Definition (Definition)
import Kitten.Fragment (Fragment)
import Kitten.Informer (Informer(..))
import Kitten.Intrinsic (Intrinsic)
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), LocalIndex(..), Qualified(..), Qualifier(..), Unqualified(..))
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Case(..), Else(..), Term(..), Permit(Permit), Value(..))
import Kitten.Vocabulary (globalVocabulary)
import qualified Data.Set as Set
import qualified Kitten.Definition as Definition
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Intrinsic as Intrinsic
import qualified Kitten.Report as Report
import qualified Kitten.Signature as Signature
import qualified Kitten.Trait as Trait
import qualified Kitten.TypeDefinition as TypeDefinition

type Resolved a = StateT [Unqualified] K a

-- Name resolution is responsible for rewriting unqualified calls to definitions
-- into fully qualified calls.

resolveNames :: Fragment () -> K (Fragment ())
resolveNames fragment = do
  reportDuplicateDefinitions
    (Set.fromList $ map Trait.name $ Fragment.traits fragment)
    $ map (Definition.name &&& Definition.origin) $ Fragment.definitions fragment
  flip evalStateT [] $ do
    definitions <- mapM resolveDefinition $ Fragment.definitions fragment
    return fragment { Fragment.definitions = definitions }
  where

  resolveDefinition :: Definition () -> Resolved (Definition ())
  resolveDefinition definition = do
    let vocabulary = qualifierName $ Definition.name definition
    body <- resolveTerm vocabulary $ Definition.body definition
    signature <- resolveSignature vocabulary $ Definition.signature definition
    return definition
      { Definition.body = body
      , Definition.signature = signature
      }

  resolveTerm :: Qualifier -> Term () -> Resolved (Term ())
  resolveTerm vocabulary = recur
    where

    recur :: Term () -> Resolved (Term ())
    recur unresolved = case unresolved of
      Call{} -> return unresolved
      Compose _ a b -> Compose () <$> recur a <*> recur b
      Drop{} -> return unresolved
      Generic{} -> error
        "generic expression should not appear before name resolution"
      Group a -> Group <$> recur a
      Identity{} -> return unresolved
      Intrinsic{} -> error
        "intrinsic name should not appear before name resolution"
      If _ a b origin -> If ()
        <$> recur a <*> recur b <*> pure origin
      Lambda _ name _ term origin -> withLocal name
        $ Lambda () name () <$> recur term <*> pure origin
      Match _ cases mElse origin -> Match ()
        <$> mapM resolveCase cases <*> traverse resolveElse mElse
        <*> pure origin
        where

        resolveCase :: Case () -> Resolved (Case ())
        resolveCase (Case name term caseOrigin) = do
          resolved <- resolveDefinitionName vocabulary name caseOrigin
          Case resolved <$> recur term <*> pure caseOrigin

        resolveElse :: Else () -> Resolved (Else ())
        resolveElse (Else term elseOrigin)
          = Else <$> recur term <*> pure elseOrigin

      New{} -> return unresolved
      NewClosure{} -> return unresolved
      NewVector{} -> return unresolved
      Push _ value origin -> Push ()
        <$> resolveValue vocabulary value <*> pure origin
      Swap{} -> return unresolved
      With _ permits origin -> With () <$> mapM resolvePermit permits
        <*> pure origin
        where
        resolvePermit (Permit allow name)
          = Permit allow <$> resolveTypeName vocabulary name origin
      Word _ fixity name params origin -> Word () fixity
        <$> resolveDefinitionName vocabulary name origin
        <*> pure params <*> pure origin

  resolveValue :: Qualifier -> Value () -> Resolved (Value ())
  resolveValue vocabulary value = case value of
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
      Signature.Application a b origin -> Signature.Application
        <$> go a <*> go b <*> pure origin
      Signature.Function as bs es origin -> Signature.Function
        <$> mapM go as <*> mapM go bs
        <*> mapM (uncurry (resolveTypeName vocabulary)) (zip es (repeat origin))
        <*> pure origin
      Signature.Quantified vars a origin -> Signature.Quantified vars
        <$> foldr withLocal (go a) (map (\ (name, _, _) -> name) vars)
        <*> pure origin
      Signature.Variable name origin -> Signature.Variable
        <$> resolveTypeName vocabulary name origin <*> pure origin
      Signature.StackFunction r as s bs es origin -> Signature.StackFunction r
        <$> mapM go as <*> pure s <*> mapM go bs
        <*> mapM (uncurry (resolveTypeName vocabulary)) (zip es (repeat origin))
        <*> pure origin

  resolveDefinitionName, resolveTypeName
    :: Qualifier -> GeneralName -> Origin -> Resolved GeneralName

  resolveDefinitionName = resolveName Report.WordName resolveLocal isDefined
    where
    isDefined = flip Set.member defined
    defined = Set.fromList $ map Definition.name $ Fragment.definitions fragment
    resolveLocal _ index = return $ LocalName index

  resolveTypeName = resolveName Report.TypeName resolveLocal isDefined
    where
    isDefined = flip Set.member defined
    defined = Set.union
      (Set.fromList
        $ map TypeDefinition.name
        $ Fragment.types fragment)
      (Set.fromList $ map Definition.name
        $ filter ((== Definition.Permission) . Definition.category)
        $ Fragment.definitions fragment)
    resolveLocal name _ = return $ UnqualifiedName name

  resolveName
    :: Report.NameCategory
    -> (Unqualified -> LocalIndex -> Resolved GeneralName)
    -> (Qualified -> Bool) -> Qualifier -> GeneralName -> Origin
    -> Resolved GeneralName
  resolveName category resolveLocal isDefined vocabulary name origin
    = case name of

-- An unqualified name may refer to a local, a name in the current vocabulary,
-- or a name in the global scope, respectively.

      UnqualifiedName unqualified -> do
        mLocalIndex <- gets (elemIndex unqualified)
        case mLocalIndex of
          Just index -> resolveLocal unqualified (LocalIndex index)
          Nothing -> do
            let qualified = Qualified vocabulary unqualified
            if isDefined qualified then return (QualifiedName qualified) else do
              let global = Qualified globalVocabulary unqualified
              if isDefined global then return (QualifiedName global) else do
                lift $ report $ Report.CannotResolveName origin category name
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
            if isDefined qualified'
              then return $ QualifiedName qualified'
              else do
                lift $ report $ Report.CannotResolveName origin category name
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
      else report $ Report.MultipleDefinitions origin name $ map snd duplicates

intrinsicFromName :: Qualified -> Maybe Intrinsic
intrinsicFromName name = case name of
  Qualified qualifier (Unqualified "+")
    | qualifier == globalVocabulary -> Just Intrinsic.Add
  Qualified qualifier (Unqualified "magic")
    | qualifier == globalVocabulary -> Just Intrinsic.Magic
  _ -> Nothing
