{-# LANGUAGE RecordWildCards #-}

module Kitten.Scope
  ( scope
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Monoid
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Kitten.Types
import Kitten.Util.List

-- Converts quotations containing references to local variables in enclosing
-- scopes into explicit closures.
scope :: Fragment ResolvedTerm -> Fragment ResolvedTerm
scope fragment@Fragment{..} = fragment
  { fragmentDefs = H.map scopeDef fragmentDefs
  , fragmentTerm = scopeTerm [0] fragmentTerm
  }

scopeDef :: Def ResolvedTerm -> Def ResolvedTerm
scopeDef def@Def{..} = def { defTerm = scopeTerm [0] <$> defTerm }

scopeTerm :: [Int] -> ResolvedTerm -> ResolvedTerm
scopeTerm stack typed = case typed of
  TrCall{} -> typed
  TrConstruct{} -> typed
  TrCompose hint terms loc -> TrCompose hint (V.map recur terms) loc
  TrIntrinsic{} -> typed
  TrLambda name term loc -> TrLambda name
    (scopeTerm (mapHead succ stack) term)
    loc
  TrMakePair as bs loc -> TrMakePair (recur as) (recur bs) loc
  TrMakeVector items loc -> TrMakeVector (V.map recur items) loc
  TrMatch cases mDefault loc -> TrMatch
    (V.map scopeCase cases) (fmap recur mDefault) loc
  TrPush value loc -> TrPush (scopeValue stack value) loc

  where
  recur :: ResolvedTerm -> ResolvedTerm
  recur = scopeTerm stack

  scopeCase (TrCase name body loc) = TrCase name (recur body) loc

scopeValue :: [Int] -> ResolvedValue -> ResolvedValue
scopeValue stack value = case value of
  TrBool{} -> value
  TrChar{} -> value
  TrClosed{} -> value
  TrClosure{} -> value
  TrFloat{} -> value
  TrInt{} -> value
  TrLocal{} -> value
  TrQuotation body x
    -> TrClosure (ClosedName <$> capturedNames) capturedTerm x
    where
    capturedTerm :: ResolvedTerm
    capturedNames :: Vector Int
    (capturedTerm, capturedNames) = runCapture stack' $ captureTerm scopedTerm
    scopedTerm :: ResolvedTerm
    scopedTerm = scopeTerm stack' body
    stack' :: [Int]
    stack' = 0 : stack
  TrText{} -> value

data Env = Env
  { envStack :: [Int]
  , envDepth :: Int
  }

type Capture a = ReaderT Env (State (Vector Int)) a

runCapture :: [Int] -> Capture a -> (a, Vector Int)
runCapture stack
  = flip runState V.empty
  . flip runReaderT Env { envStack = stack, envDepth = 0 }

addName :: Int -> Capture Int
addName name = do
  names <- lift get
  case V.elemIndex name names of
    Just existing -> return existing
    Nothing -> do
      lift $ put (names <> V.singleton name)
      return $ V.length names

captureTerm :: ResolvedTerm -> Capture ResolvedTerm
captureTerm typed = case typed of
  TrCall{} -> return typed
  TrCompose hint terms loc -> TrCompose hint
    <$> V.mapM captureTerm terms
    <*> pure loc
  TrConstruct{} -> return typed
  TrIntrinsic{} -> return typed
  TrLambda name terms loc -> let
    inside env@Env{..} = env
      { envStack = mapHead succ envStack
      , envDepth = succ envDepth
      }
    in TrLambda name
      <$> local inside (captureTerm terms)
      <*> pure loc
  TrMakePair a b loc -> TrMakePair
    <$> captureTerm a
    <*> captureTerm b
    <*> pure loc
  TrMakeVector items loc -> TrMakeVector
    <$> V.mapM captureTerm items
    <*> pure loc
  TrMatch cases mDefault loc -> TrMatch
    <$> V.mapM captureCase cases
    <*> maybe (pure Nothing) (fmap Just . captureTerm) mDefault
    <*> pure loc
    where
    captureCase (TrCase name body loc') = TrCase name
      <$> captureTerm body
      <*> pure loc'
  TrPush value loc -> TrPush <$> captureValue value <*> pure loc

closeLocal :: Int -> Capture (Maybe Int)
closeLocal index = do
  stack <- asks envStack
  depth <- asks envDepth
  case stack of
    (here : _)
      | index >= here
      -> Just <$> addName (index - depth)
    _ -> return Nothing

captureValue :: ResolvedValue -> Capture ResolvedValue
captureValue value = case value of
  TrBool{} -> return value
  TrChar{} -> return value
  TrClosed{} -> return value
  TrClosure names term x -> TrClosure
    <$> V.mapM close names
    <*> pure term
    <*> pure x
    where
    close :: ClosedName -> Capture ClosedName
    close original@(ClosedName name) = do
      closed <- closeLocal name
      return $ case closed of
        Nothing -> original
        Just closedLocal -> ReclosedName closedLocal
    close original@(ReclosedName _) = return original
  TrFloat{} -> return value
  TrInt{} -> return value
  TrQuotation terms x -> let
    inside env@Env{..} = env { envStack = 0 : envStack }
    in TrQuotation <$> local inside (captureTerm terms) <*> pure x
  TrLocal name x -> do
    closed <- closeLocal name
    return $ case closed of
      Nothing -> value
      Just closedName -> TrClosed closedName x
  TrText{} -> return value
