{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Resolve
  ( resolve
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Def
import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve.Monad
import Kitten.Resolved
import Kitten.Term (Term)

import qualified Kitten.Term as Term

resolve
  :: [Def Resolved]
  -> Fragment Term
  -> Either CompileError (Fragment Resolved)
resolve prelude (Fragment defs terms) = do
  -- TODO Don't fail so eagerly.
  flip evalStateT emptyEnv $ Fragment
    <$> resolveDefs defs
    <*> mapM resolveTerm terms
  where emptyEnv = Env prelude defs []

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = mapM resolveDef defs
  where
  resolveDef (Def name body loc)
    = Def name <$> resolveTerm body <*> pure loc

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Push value _ -> resolveValue value
  Term.Builtin name loc -> return $ Builtin name loc
  Term.Block terms -> Block
    <$> mapM resolveTerm terms
  Term.Lambda name terms loc -> withLocal name
    $ Scoped <$> mapM resolveTerm terms <*> pure loc
  Term.If condition true false loc -> If
    <$> mapM resolveTerm condition
    <*> mapM resolveTerm true
    <*> mapM resolveTerm false
    <*> pure loc

fromValue :: Resolved -> Value
fromValue (Push value _) = value
fromValue _ = error "Resolve.fromValue: not a value"

resolveValue :: Term.Value -> Resolution Resolved
resolveValue unresolved = case unresolved of
  Term.Escape name loc -> resolveName Escape name loc
  Term.Float value loc -> return $ Push (Float value) loc
  Term.Function anno term loc -> Push . Function anno
    <$> mapM resolveTerm term <*> pure loc
  Term.Int value loc -> return $ Push (Int value) loc
  Term.Bool value loc -> return $ Push (Bool value) loc
  Term.Char value loc -> return $ Push (Char value) loc
  Term.Pair a b loc -> do
    a' <- fromValue <$> resolveValue a
    b' <- fromValue <$> resolveValue b
    return $ Push (Pair a' b') loc
  Term.Unit loc -> return $ Push Unit loc
  Term.Vector anno values loc -> Push . Vector anno
    <$> resolveVector values <*> pure loc
  Term.Word name loc -> resolveName Word name loc
  where
  resolveVector = mapM $ fmap fromValue . resolveValue

resolveName
  :: (Set Name -> Value)
  -> String
  -> Location
  -> Resolution Resolved
resolveName wrap name loc = do
  mLocalIndex <- gets $ localIndex name
  case mLocalIndex of
    Just index -> return $ Local (Name index) loc
    Nothing -> do
      indices <- gets $ defIndices name
      when (null indices)
        . lift . Left . CompileError loc $ concat
          ["unable to resolve word '", name, "'"]
      return $ Push
        (wrap . Set.fromList $ map Name indices)
        loc
