{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Kitten.Resolve
  ( resolve
  , search
  , searchAbbrev
  ) where

import Control.Arrow
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import GHC.Exts

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Definition
import Kitten.Error
import Kitten.Fragment
import Kitten.Intrinsic
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Program
import Kitten.Resolve.Monad
import Kitten.Term
import Kitten.Util.List
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..))

resolve
  :: Fragment ParsedTerm
  -> Program
  -> Either [ErrorGroup] (Fragment ResolvedTerm)
resolve fragment program = do
  case reportDuplicateDefs allNamesAndLocs of
    [] -> noop
    errors -> Left errors
  evalResolution emptyEnv $ guardLiftM2
    (\defs term -> fragment
      { fragmentDefs = defs
      , fragmentTerm = term
      })
    (resolveDefs (fragmentDefs fragment))
    (resolveTerm (fragmentTerm fragment))
  where
  allNamesAndLocs = map (fst &&& defLocation . snd) . H.toList
    $ fragmentDefs fragment
  emptyEnv = Env
    { envAbbrevs = let x = fragmentAbbrevs fragment <> programAbbrevs program in x  -- traceShow x x
    , envDefined = mconcat
      [ S.fromList $ H.keys (programSymbols program)
      , S.fromList $ H.keys (fragmentDefs fragment)
      ]
    , envScope = []
    , envVocabulary = Qualifier V.empty
    }

reportDuplicateDefs
  :: [(Name, Location)]
  -> [ErrorGroup]
reportDuplicateDefs param = mapMaybe reportDuplicate . groupWith fst $ param
  where
  reportDuplicate defs = case defs of
    [] -> Nothing
    [_] -> Nothing
    ((name, loc) : duplicates) -> Just . ErrorGroup
      $ CompileError loc Error ("duplicate definition of " <> toText name)
      : for duplicates
        (\ (_, here) -> CompileError here Note "also defined here")

resolveDefs
  :: HashMap Name (Def ParsedTerm)
  -> Resolution (HashMap Name (Def ResolvedTerm))
resolveDefs = guardMapM resolveDef
  where
  resolveDef :: Def ParsedTerm -> Resolution (Def ResolvedTerm)
  resolveDef def = do
    let
      vocabulary = case defName def of
        Qualified qualifier _ -> qualifier
        MixfixName{} -> Qualifier V.empty
        _ -> error $ concat
          [ "definition '"
          , show (defName def)
          , "' appeared outside vocabulary"
          ]
    original <- getsEnv envVocabulary
    modifyEnv $ \env -> env { envVocabulary = vocabulary }
    defTerm' <- traverse resolveTerm (defTerm def)
    modifyEnv $ \env -> env { envVocabulary = original }
    return def { defTerm = defTerm' }

resolveTerm :: ParsedTerm -> Resolution ResolvedTerm
resolveTerm unresolved = case unresolved of
  TrCall fixity name loc -> resolveName fixity name loc
  TrCompose hint terms loc -> TrCompose hint
    <$> guardMapM resolveTerm terms
    <*> pure loc
  TrConstruct name ctor size loc -> return $ TrConstruct name ctor size loc
  TrIntrinsic name loc -> return $ TrIntrinsic name loc
  TrLambda name nameLoc term loc -> withLocal name $ TrLambda name nameLoc
    <$> resolveTerm term
    <*> pure loc
  TrMakePair as bs loc -> TrMakePair
    <$> resolveTerm as
    <*> resolveTerm bs
    <*> pure loc
  TrMakeVector items loc -> TrMakeVector
    <$> guardMapM resolveTerm items
    <*> pure loc
  TrMatch cases mDefault loc -> TrMatch
    <$> guardMapM resolveCase cases
    <*> traverse resolveValue mDefault
    <*> pure loc
    where
    resolveCase (TrCase name body loc') = do
      vocab <- getsEnv envVocabulary
      abbrevs <- getsEnv envAbbrevs
      let
        lookupAbbrev x = H.lookup x abbrevs
        unknown = compileError . oneError . CompileError loc' Error
          $ T.concat ["undefined constructor '", toText name, "'"]
      resolved <- case name of
        Unqualified text -> search vocab text isDefined return unknown
        Qualified qualifier text -> searchAbbrev vocab qualifier text isDefined lookupAbbrev return unknown
        _ -> error "constructor names cannot be mixfix"
      TrCase resolved <$> resolveValue body <*> pure loc'
  TrPush value loc -> TrPush <$> resolveValue value <*> pure loc

resolveValue :: ParsedValue -> Resolution ResolvedValue
resolveValue unresolved = case unresolved of
  TrBool value loc -> return $ TrBool value loc
  TrChar value loc -> return $ TrChar value loc
  TrClosed{} -> error "FIXME 'Closed' appeared before resolution"
  TrClosure{} -> error "FIXME 'Closure' appeared before resolution"
  TrFloat value loc -> return $ TrFloat value loc
  TrInt value loc -> return $ TrInt value loc
  TrLocal{} -> error "FIXME 'Local' appeared before resolution"
  TrQuotation term loc -> TrQuotation
    <$> resolveTerm term <*> pure loc
  TrText value loc -> return $ TrText value loc

search
  :: (Monad m)
  => Qualifier
  -> Text
  -> (Name -> m Bool)
  -> (Name -> m a)
  -> m a
  -> m a
search vocab name defined call default_ = do
  let name' = Unqualified name
  presentUnqualified <- defined name'
  if presentUnqualified then call name' else do
  let here = Qualified vocab name
  presentHere <- defined here
  if presentHere then call here else do
  let globally = Qualified (Qualifier V.empty) name
  presentGlobally <- defined globally
  if presentGlobally then call globally else default_

searchAbbrev
  :: (Monad m)
  => Qualifier
  -> Qualifier
  -> Text
  -> (Name -> m Bool)
  -> ((Qualifier, Text) -> Maybe Qualifier)
  -> (Name -> m b)
  -> m b
  -> m b
searchAbbrev vocab qualifier name defined lookupAbbrev call default_ = do
  let name' = Qualified qualifier name
  presentUnabbreviated <- defined name'
  if presentUnabbreviated then call name' else case qualifier of
    Qualifier q
      | V.length q == 1 -> case lookupAbbrev (vocab, (q V.! 0)) of
        Just long -> do
          let expanded = Qualified long name
          presentExpanded <- defined expanded
          if presentExpanded then call expanded
            else case lookupAbbrev (Qualifier V.empty, (q V.! 0)) of
              Just long' -> do
                let globally = Qualified long' name
                presentGlobally <- defined globally
                if presentGlobally then call globally else default_
              Nothing -> default_
        Nothing -> default_
      | otherwise -> default_

resolveName
  :: Fixity
  -> Name
  -> Location
  -> Resolution ResolvedTerm
resolveName fixity name loc = do
  vocab <- getsEnv envVocabulary
  abbrevs <- getsEnv envAbbrevs
  let lookupAbbrev x = H.lookup x abbrevs
  mLocalIndex <- getsEnv $ localIndex name
  case mLocalIndex of
    Just index -> return $ TrPush (TrLocal index loc) loc
    Nothing -> case name of
      Unqualified text -> search vocab text isDefined call intrinsic
      Qualified qualifier text -> searchAbbrev vocab qualifier text isDefined lookupAbbrev call intrinsic
      _ -> do
        present <- getsEnv $ S.member name . envDefined
        if present then call name else intrinsic
  where
  intrinsic = case intrinsicFromName name of
    Just i -> return $ TrIntrinsic i loc
    Nothing -> failure
  call n = return $ TrCall fixity n loc
  failure = compileError . oneError . CompileError loc Error
    $ T.concat ["undefined word '", toText name, "'"]

isDefined :: Name -> Resolution Bool
isDefined x = getsEnv $ S.member x . envDefined
