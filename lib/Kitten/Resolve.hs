{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Resolve
  ( resolve
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable (forM_)
import Data.Monoid

import Kitten.Def
import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve.Monad
import Kitten.Resolved
import Kitten.Term (Term)
import Kitten.Util.Applicative

import qualified Kitten.Term as Term

resolve
  :: [Def Resolved]
  -> Fragment Term
  -> Either CompileError (Fragment Resolved)
resolve prelude (Fragment defs terms) = do
  -- TODO Don't fail so eagerly.
  resolveDuplicateDefs defs
  flip evalStateT emptyEnv $ Fragment
    <$> resolveDefs defs
    <*> mapM resolveTerm terms
  where emptyEnv = Env prelude defs []

resolveDuplicateDefs
  :: [Def Term]
  -> Either CompileError ()
resolveDuplicateDefs defs = forM_ defs $ \ def
  -> case filter (duplicate def) defs of
    [] -> Right ()
    duplicates -> Left $ DuplicateError
      (defLocation def)
      (map defLocation duplicates)
      (defName def)

  where
  duplicate :: Def Term -> Def Term -> Bool
  duplicate def
    = (== defName def) . defName
    .&&. (/= defLocation def) . defLocation

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = (<>) <$> gets envPrelude <*> mapM resolveDef defs
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
  Term.Text value loc -> return $ Push (Text value) loc
  Term.Vector anno terms loc -> Push . Vector anno
    <$> resolveVector terms <*> pure loc
  Term.Word name loc -> resolveName Word name loc
  where
  resolveVector = mapM $ fmap fromValue . resolveValue

resolveName
  :: (Name -> Value)
  -> String
  -> Location
  -> Resolution Resolved
resolveName wrap name loc = do
  mLocalIndex <- gets $ localIndex name
  case mLocalIndex of
    Just index -> return $ Local (Name index) loc
    Nothing -> do
      mDefIndex <- gets $ defIndex name
      case mDefIndex of
        Just index -> return
          $ Push (wrap $ Name index) loc
        Nothing -> lift . Left . CompileError loc $ concat
          ["unable to resolve word '", name, "'"]
