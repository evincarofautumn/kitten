{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Resolve
  ( resolve
  ) where

import Control.Applicative hiding (some)
import Control.Arrow
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)
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
    { envDefined = mconcat
      [ S.fromList $ H.keys (programSymbols program)
      , S.fromList $ H.keys (fragmentDefs fragment)
      ]
    , envScope = []
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
    defTerm' <- traverse resolveTerm (defTerm def)
    return def { defTerm = defTerm' }

resolveTerm :: ParsedTerm -> Resolution ResolvedTerm
resolveTerm unresolved = case unresolved of
  TrCall fixity name loc -> resolveName fixity name loc
  TrCompose hint terms loc -> TrCompose hint
    <$> guardMapM resolveTerm terms
    <*> pure loc
  TrConstruct name ctor size loc -> return $ TrConstruct name ctor size loc
  TrIntrinsic name loc -> return $ TrIntrinsic name loc
  TrLambda name term loc -> withLocal name $ TrLambda name
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
    resolveCase (TrCase name body loc') = TrCase name
      <$> resolveValue body
      <*> pure loc'
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
  TrQuotation term loc -> TrQuotation <$> resolveTerm term <*> pure loc
  TrText value loc -> return $ TrText value loc

resolveName
  :: Fixity
  -> Name
  -> Location
  -> Resolution ResolvedTerm
resolveName fixity name loc = do
  mLocalIndex <- getsEnv $ localIndex name
  case mLocalIndex of
    Just index -> return $ TrPush (TrLocal index loc) loc
    Nothing -> do
      present <- getsEnv $ S.member name . envDefined
      if present then call else case intrinsicFromName name of
        Just intrinsic -> return $ TrIntrinsic intrinsic loc
        Nothing -> failure
  where
  call = return $ TrCall fixity name loc
  failure = compileError . oneError . CompileError loc Error
    $ T.concat ["undefined word '", toText name, "'"]
