{-# LANGUAGE OverloadedStrings #-}

module Kitten.Resolve
  ( resolve
  ) where

import Control.Applicative hiding (some)
import Control.Arrow
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Exts

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolve.Monad
import Kitten.Resolved
import Kitten.Term (Term)
import Kitten.Typed (Typed)
import Kitten.Util.Function

import qualified Kitten.Term as Term

resolve
  :: Fragment Typed
  -> Fragment Term
  -> Either [ErrorGroup] (Fragment Resolved)
resolve prelude fragment = do
  case reportDuplicateDefs allNamesAndLocs of
    [] -> return ()
    errors -> Left errors
  evalResolution emptyEnv $ guardLiftM2
    (\defs terms -> fragment
      { fragmentDefs = defs
      , fragmentTerms = terms
      })
    (resolveDefs (fragmentDefs fragment))
    (guardMapM resolveTerm (fragmentTerms fragment))
  where
  allNamesAndLocs
    = namesAndLocs (fragmentDefs prelude)
    ++ namesAndLocs (fragmentDefs fragment)
  emptyEnv = Env
    (fragmentDefs prelude)
    (fragmentDefs fragment)
    []

namesAndLocs :: Vector (Def a) -> [(Text, Location)]
namesAndLocs = map (defName &&& defLocation) . V.toList

reportDuplicateDefs
  :: [(Text, Location)]
  -> [ErrorGroup]
reportDuplicateDefs param = mapMaybe reportDuplicate . groupWith fst $ param
  where
  reportDuplicate defs = case defs of
    [] -> Nothing
    [_] -> Nothing
    ((name, loc) : duplicates) -> Just . ErrorGroup
      $ CompileError loc Error ("duplicate definition of " <> name)
      : for duplicates
        (\ (_, here) -> CompileError here Note "also defined here")

resolveDefs :: Vector (Def Term) -> Resolution (Vector (Def Resolved))
resolveDefs = guardMapM resolveDef
  where
  resolveDef :: Def Term -> Resolution (Def Resolved)
  resolveDef def = do
    defTerm' <- resolveTerm (defTerm def)
    return def { defTerm = defTerm' }

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Builtin name loc -> return $ Builtin name loc
  Term.Call name loc -> resolveName name loc
  Term.Compose terms loc -> Compose
    <$> guardMapM resolveTerm terms
    <*> pure loc
  Term.Lambda name term loc -> withLocal name $ Scoped name
    <$> resolveTerm term
    <*> pure loc
  Term.PairTerm as bs loc -> PairTerm
    <$> resolveTerm as
    <*> resolveTerm bs
    <*> pure loc
  Term.Push value loc -> Push <$> resolveValue value <*> pure loc
  Term.VectorTerm items loc -> VectorTerm
    <$> guardMapM resolveTerm items
    <*> pure loc

resolveValue :: Term.Value -> Resolution Value
resolveValue unresolved = case unresolved of
  Term.Bool value _ -> return $ Bool value
  Term.Char value _ -> return $ Char value
  Term.Float value _ -> return $ Float value
  Term.Function term loc -> Function
    <$> (Compose <$> guardMapM resolveTerm term <*> pure loc)
  Term.Int value _ -> return $ Int value
  Term.Unit _ -> return Unit
  Term.String value _ -> return $ String value

resolveName
  :: Text
  -> Location
  -> Resolution Resolved
resolveName name loc = do
  mLocalIndex <- getsEnv $ localIndex name
  case mLocalIndex of
    Just index -> return $ Push (Local $ Name index) loc
    Nothing -> do
      indices <- getsEnv $ defIndices name
      index <- case V.toList indices of
        [index] -> return index
        [] -> err ["undefined word '", name, "'"]
        _ -> err ["ambiguous word '", name, "'"]
      return $ Call (Name index) loc
  where
  err = compileError . oneError . CompileError loc Error . T.concat
