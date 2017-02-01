{-|
Module      : Kitten.Resolve
Description : Name resolution
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

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
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
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

definition :: Dictionary -> Definition () -> Resolved (Definition ())
definition dictionary def = do
  -- FIXME: reportDuplicate dictionary def
  let vocabulary = qualifierName $ Definition.name def
  sig <- signature dictionary vocabulary [] $ Definition.signature def
  let
    parameters = case sig of
      Signature.Quantified params _ _ -> params
      _ -> []
  body <- term dictionary vocabulary parameters $ Definition.body def
  return def
    { Definition.body = body
    , Definition.signature = sig
    }

term
  :: Dictionary
  -> Qualifier
  -> [Parameter]
  -> Term ()
  -> Resolved (Term ())
term dictionary vocabulary parameters = recur
  where

  recur :: Term () -> Resolved (Term ())
  recur unresolved = case unresolved of
    Coercion (Term.AnyCoercion sig) a b
      -> Coercion
      <$> (Term.AnyCoercion <$> signature dictionary vocabulary parameters sig)
      <*> pure a
      <*> pure b
    Coercion{} -> return unresolved
    Compose _ a b -> Compose () <$> recur a <*> recur b
    Generic{} -> error
      "generic expression should not appear before name resolution"
    Lambda _ name _ t origin -> withLocal name
      $ Lambda () name () <$> recur t <*> pure origin
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
      <$> value dictionary vocabulary parameters v <*> pure origin
    Word _ fixity name params origin -> Word () fixity
      <$> definitionName dictionary vocabulary name origin
      <*> pure params <*> pure origin

value
  :: Dictionary
  -> Qualifier
  -> [Parameter]
  -> Value ()
  -> Resolved (Value ())
value dictionary vocabulary parameters v = case v of
  Algebraic{} -> error "adt should not appear before runtime"
  Array{} -> error "array should not appear before runtime"
  Capture{} -> error "closure should not appear before name resolution"
  Character{} -> return v
  Closed{} -> error "closed name should not appear before name resolution"
  Closure{} -> error "closure should not appear before runtime"
  Float{} -> return v
  Integer{} -> return v
  Local{} -> error "local name should not appear before name resolution"
  -- FIXME: Maybe should be a GeneralName and require resolution.
  Name{} -> return v
  Quotation t -> Quotation <$> term dictionary vocabulary parameters t
  Text{} -> return v

signature
  :: Dictionary
  -> Qualifier
  -> [Parameter]
  -> Signature
  -> Resolved Signature
signature dictionary vocabulary parameters = withParameters parameters
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
      <$> withParameters vars a
      <*> pure origin
    Signature.Variable name origin -> Signature.Variable
      <$> typeName dictionary vocabulary name origin <*> pure origin
    Signature.StackFunction r as s bs es origin -> Signature.StackFunction r
      <$> mapM go as <*> pure s <*> mapM go bs
      <*> zipWithM (typeName dictionary vocabulary) es (repeat origin)
      <*> pure origin
    Signature.Type{} -> pure sig

  withParameters vars x = foldr
    (withLocal . (\ (Parameter _ name _) -> name))
    (go x) vars

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

withLocal :: Unqualified -> Resolved a -> Resolved a
withLocal name action = do
  modify (name :)
  result <- action
  modify tail
  return result

reportDuplicate :: Dictionary -> Definition () -> K ()
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
