{-|
Module      : Kitten.Resolve
Description : Name resolution
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Resolve
  ( definition
  , generalName
  , run
  , signature
  ) where

import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.List (elemIndex)
import Kitten.Definition (Definition)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Informer (Informer(..))
import Kitten.Monad (K)
import Kitten.Name
import Kitten.Operator (Fixity)
import Kitten.Origin (Origin)
import Kitten.Phase (Phase(..))
import Kitten.Signature (Signature)
import Kitten.Term (Sweet(..))
import qualified Data.Set as Set
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Report as Report
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary

type Resolved a = StateT [Unqualified] K a

-- | Name resolution is responsible for rewriting unqualified calls to
-- definitions into fully qualified calls.

run :: Resolved a -> K a
run = flip evalStateT []

definition
  :: Dictionary
  -> Definition 'Parsed
  -> Resolved (Definition 'Resolved)
definition dictionary def = do
  -- FIXME: reportDuplicate dictionary def
  let vocabulary = qualifierName $ Definition.name def
  body <- term dictionary vocabulary $ Definition.body def
  sig <- signature dictionary vocabulary $ Definition.signature def
  return def
    { Definition.body = body
    , Definition.signature = sig
    }

term
  :: Dictionary
  -> Qualifier
  -> Sweet 'Parsed
  -> Resolved (Sweet 'Resolved)
term dictionary vocabulary = recur
  where

  recur :: Sweet 'Parsed -> Resolved (Sweet 'Resolved)
  recur unresolved = case unresolved of
    SArray _ origin items -> SArray () origin <$> mapM recur items
    SAs _ origin types -> SAs () origin
      <$> mapM (signature dictionary vocabulary) types
    SCharacter _ origin text -> pure $ SCharacter () origin text
    SCompose _ a b -> SCompose () <$> recur a <*> recur b
    SDo _ origin f x -> SDo () origin <$> recur f <*> recur x
    SEscape _ origin body -> SEscape () origin <$> recur body
    SFloat _ origin literal -> pure $ SFloat () origin literal
    SGeneric{} -> error "generic term should not appear before name resolution"
    SGroup _ origin body -> SGroup () origin <$> recur body
    SIdentity _ origin -> pure $ SIdentity () origin
    SIf _ origin mCondition true elifs mElse -> SIf () origin
      <$> traverse recur mCondition
      <*> recur true
      <*> traverse (\ (elifOrigin, condition, body)
        -> (,,) elifOrigin <$> recur condition <*> recur body) elifs
      <*> traverse recur mElse
    -- SInfix{}
    SInteger _ origin literal -> pure $ SInteger () origin literal
    SJump _ origin -> pure $ SJump () origin
    SLambda _ origin vars body -> withLocals vars
      $ SLambda () origin vars <$> recur body
    SList _ origin items -> SList () origin <$> mapM recur items
    -- SLocal _ origin name -> pure $ SLocal () origin name
    SLoop _ origin -> pure $ SLoop () origin
    SMatch _ origin mScrutinee cases mElse -> SMatch () origin
      <$> traverse recur mScrutinee
      <*> traverse (\ (caseOrigin, name, body) -> (,,) origin
        <$> definitionName dictionary vocabulary name caseOrigin
        <*> recur body)
        cases
      <*> traverse recur mElse
    SNestableCharacter _ origin text -> pure $ SNestableCharacter () origin text
    SNestableText _ origin text -> pure $ SNestableText () origin text
    SParagraph _ origin text -> pure $ SParagraph () origin text
    STag _ origin size index -> pure $ STag () origin size index
    SText _ origin text -> pure $ SText () origin text
    SQuotation _ origin body -> SQuotation () origin <$> recur body
    SReturn _ origin -> pure $ SReturn () origin
    SSection _ origin name operand -> SSection () origin name  -- TODO: name
      <$> case operand of
        Left x -> Left <$> recur x
        Right x -> Right <$> recur x
    STodo _ origin -> pure $ STodo () origin
    SUnboxedQuotation _ origin body
      -> SUnboxedQuotation () origin <$> recur body
    SWith _ origin permits -> SWith () origin <$> mapM permit permits
      where
        permit (Term.Permit permitted name) = Term.Permit permitted
          <$> typeName dictionary vocabulary name origin
    SWord _ origin fixity name typeArgs
      -> word dictionary vocabulary origin fixity typeArgs name
{-
      -> SWord () origin fixity
        <$> definitionName dictionary vocabulary name origin
        <*> mapM (signature dictionary vocabulary) typeArgs
-}

{-
    Match hint _ cases else_ origin -> Match hint ()
      <$> mapM resolveCase cases <*> resolveElse else_
      <*> pure origin
      where

      resolveCase :: Case () -> Resolved (Case ())
      resolveCase (Case name t caseOrigin) = do
        resolved <- definitionName dictionary vocabulary name caseOrigin
        Case resolved <$> recur t <*> pure caseOrigin

      resolveElse :: Else () -> Resolved (Else ())
      resolveElse (Else t elseOrigin)
        = Else <$> recur t <*> pure elseOrigin

    New{} -> return unresolved
    NewClosure{} -> return unresolved
    NewVector{} -> return unresolved
    Push _ v origin -> Push ()
      <$> value dictionary vocabulary v <*> pure origin
    Word _ fixity name params origin -> Word () fixity
      <$> definitionName dictionary vocabulary name origin
      <*> pure params <*> pure origin
-}

signature :: Dictionary -> Qualifier -> Signature -> Resolved Signature
signature dictionary vocabulary = go
  where

  go :: Signature -> Resolved Signature
  go sig = case sig of
    Signature.Application a b origin -> Signature.Application
      <$> go a <*> go b <*> pure origin
    Signature.Bottom{} -> pure sig
    Signature.Function as bs es origin -> Signature.Function
      <$> mapM go as <*> mapM go bs
      <*> zipWithM (typeName dictionary vocabulary) es (repeat origin)
      <*> pure origin
    Signature.Quantified vars a origin -> Signature.Quantified vars
      <$> foldr (withLocal . (\ (Parameter _ name _) -> name)) (go a) vars
      <*> pure origin
    Signature.Variable name origin -> Signature.Variable
      <$> typeName dictionary vocabulary name origin <*> pure origin
    Signature.StackFunction r as s bs es origin -> Signature.StackFunction r
      <$> mapM go as <*> pure s <*> mapM go bs
      <*> zipWithM (typeName dictionary vocabulary) es (repeat origin)
      <*> pure origin
    Signature.Type{} -> pure sig

word
  :: Dictionary
  -> Qualifier
  -> Origin
  -> Fixity
  -> [Signature]
  -> GeneralName
  -> Resolved (Sweet 'Resolved)
word dictionary vocabulary origin fixity typeArgs name = do
  name' <- generalName Report.WordName resolveLocal isDefined
    vocabulary name origin
  pure $ case name' of
    -- FIXME: Should we report if a local name is given type arguments?
    UnqualifiedName unqualified -> SLocal () origin unqualified
    _ -> SWord () origin fixity name' typeArgs
  where
  isDefined = flip Set.member defined
  defined = Set.fromList $ Dictionary.wordNames dictionary
  resolveLocal unqualified _index = return $ UnqualifiedName unqualified

definitionName, typeName
  :: Dictionary -> Qualifier -> GeneralName -> Origin -> Resolved GeneralName

definitionName dictionary
  = generalName Report.WordName resolveLocal isDefined
  where
  isDefined = flip Set.member defined
  defined = Set.fromList $ Dictionary.wordNames dictionary
  resolveLocal _ index = return $ LocalName index

typeName dictionary
  = generalName Report.TypeName resolveLocal isDefined
  where
  isDefined = flip Set.member defined
  defined = Set.fromList $ Dictionary.typeNames dictionary
  resolveLocal local _ = return $ UnqualifiedName local

generalName
  :: Report.NameCategory
  -> (Unqualified -> LocalIndex -> Resolved GeneralName)
  -> (Qualified -> Bool) -> Qualifier -> GeneralName -> Origin
  -> Resolved GeneralName
generalName category resolveLocal isDefined vocabulary name origin
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
            let global = Qualified Vocabulary.global unqualified
            if isDefined global then return (QualifiedName global) else do
              lift $ report $ Report.CannotResolveName origin category name
              return name

-- A qualified name may refer to an intrinsic or a definition.

    QualifiedName qualified -> if isDefined qualified then return name else do
      let
        qualified' = case (vocabulary, qualifierName qualified) of
          (Qualifier _root1 prefix, Qualifier _root2 suffix)
            -> Qualified (Qualifier Absolute (prefix ++ suffix))
              $ unqualifiedName qualified
      if isDefined qualified'
        then return $ QualifiedName qualified'
        else do
          lift $ report $ Report.CannotResolveName origin category name
          return name

    LocalName{} -> error "local name should not appear before name resolution"

withLocals :: [(Origin, Maybe Unqualified, ())] -> Resolved a -> Resolved a
withLocals ((_, Just name, _) : locals) action
  = withLocal name $ withLocals locals action
-- FIXME: Is this correct?
withLocals ((_, Nothing, _) : locals) action = withLocals locals action
withLocals [] action = action

withLocal :: Unqualified -> Resolved a -> Resolved a
withLocal name action = do
  modify (name :)
  result <- action
  modify tail
  return result

reportDuplicate :: Dictionary -> Definition p -> K ()
reportDuplicate dictionary def = return ()
{-
  where

  go :: [(Qualified, Origin)] -> K ()
  go defs = case defs of
    [] -> return ()
    [_] -> return ()
    ((name, origin) : duplicates) -> if name `Set.member` generic
      then return ()
      else report $ Report.MultipleDefinitions origin name $ map snd duplicates
-}
