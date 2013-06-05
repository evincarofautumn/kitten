{-# LANGUAGE OverloadedStrings #-}

module Kitten.Resolve
  ( resolve
  ) where

import Control.Applicative

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolve.Monad
import Kitten.Resolved
import Kitten.Term (Term)

import qualified Kitten.Typed as Typed
import qualified Kitten.Term as Term

resolve
  :: [Def Typed.Value]
  -> Fragment Term.Value Term
  -> Either [CompileError] (Fragment Value Resolved)
resolve prelude (Fragment defs terms)
  = evalResolution emptyEnv $ guardLiftM2 Fragment
    (resolveDefs defs)
    (guardMapM resolveTerm terms)
  where emptyEnv = Env prelude defs []

resolveDefs :: [Def Term.Value] -> Resolution [Def Value]
resolveDefs = guardMapM resolveDef
  where
  resolveDef :: Def Term.Value -> Resolution (Def Value)
  resolveDef (Def name body loc)
    = Def name <$> resolveValue body <*> pure loc

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Call name loc -> resolveName name loc
  Term.Push value loc -> Push <$> resolveValue value <*> pure loc
  Term.Builtin name loc -> return $ Builtin name loc
  Term.Block terms -> Block
    <$> guardMapM resolveTerm terms
  Term.Lambda name terms loc -> withLocal name
    $ Scoped <$> guardMapM resolveTerm terms <*> pure loc
  Term.If condition true false loc -> If
    <$> guardMapM resolveTerm condition
    <*> guardMapM resolveTerm true
    <*> guardMapM resolveTerm false
    <*> pure loc

resolveValue :: Term.Value -> Resolution Value
resolveValue unresolved = case unresolved of
  Term.Escape name loc -> resolveEscape name loc
  Term.Float value _ -> return $ Float value
  Term.Function anno term _ -> Function anno
    <$> guardMapM resolveTerm term
  Term.Int value _ -> return $ Int value
  Term.Bool value _ -> return $ Bool value
  Term.Char value _ -> return $ Char value
  Term.Pair a b _ -> do
    a' <- resolveValue a
    b' <- resolveValue b
    return $ Pair a' b'
  Term.Unit _ -> return Unit
  Term.Vector anno values _ -> Vector anno
    <$> resolveVector values
  where
  resolveVector = guardMapM resolveValue

resolveName
  :: String
  -> Location
  -> Resolution Resolved
resolveName name loc = do
  mLocalIndex <- getsEnv $ localIndex name
  case mLocalIndex of
    Just index -> return $ Push (Local $ Name index) loc
    Nothing -> do
      indices <- getsEnv $ defIndices name
      index <- case indices of
        [index] -> return index
        [] -> compileError . CompileError loc $ unwords
          ["undefined word:", name]
        _ -> compileError . CompileError loc $ unwords
          ["ambiguous word:", name]
      return $ Call (Name index) loc

resolveEscape
  :: String
  -> Location
  -> Resolution Value
resolveEscape name loc = do
  indices <- getsEnv $ defIndices name
  index <- case indices of
    [index] -> return index
    [] -> compileError . CompileError loc $ unwords
      ["undefined word:", name]
    _ -> compileError . CompileError loc $ unwords
      ["ambiguous word:", name]
  return $ Escape (Name index)
