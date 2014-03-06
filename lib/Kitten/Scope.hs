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

import qualified Data.Traversable as T
import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Tree
import Kitten.Util.List

scope :: Fragment ResolvedTerm -> Fragment ResolvedTerm
scope fragment@Fragment{..} = fragment
  { fragmentDefs = V.map scopeDef fragmentDefs
  , fragmentTerms = V.map (scopeTerm [0]) fragmentTerms
  }

scopeDef :: Def ResolvedTerm -> Def ResolvedTerm
scopeDef def@Def{..} = def { defTerm = scopeTerm [0] <$> defTerm }

scopeTerm :: [Int] -> ResolvedTerm -> ResolvedTerm
scopeTerm stack typed = case typed of
  Builtin{} -> typed
  Call{} -> typed
  Compose terms loc -> Compose (recur <$> terms) loc
  Lambda name term loc -> Lambda name
    (scopeTerm (mapHead succ stack) term)
    loc
  PairTerm as bs loc -> PairTerm (recur as) (recur bs) loc
  Push value loc -> Push (scopeValue stack value) loc
  VectorTerm items loc -> VectorTerm (recur <$> items) loc

  where
  recur :: ResolvedTerm -> ResolvedTerm
  recur = scopeTerm stack

scopeValue :: [Int] -> ResolvedValue -> ResolvedValue
scopeValue stack value = case value of
  Bool{} -> value
  Char{} -> value
  Closed{} -> value
  Closure{} -> value
  Float{} -> value

  Function funTerm x
    -> Closure (ClosedName <$> capturedNames) capturedTerm x
    where

    capturedTerm :: ResolvedTerm
    capturedNames :: Vector Name
    (capturedTerm, capturedNames)
      = runCapture stack'
      $ captureTerm scopedTerm

    scopedTerm :: ResolvedTerm
    scopedTerm = scopeTerm stack' funTerm

    stack' :: [Int]
    stack' = 0 : stack

  Int{} -> value
  Local{} -> value
  Unit{} -> value
  String{} -> value

data Env = Env
  { envStack :: [Int]
  , envDepth :: Int
  }

type Capture a = ReaderT Env (State (Vector Name)) a

runCapture :: [Int] -> Capture a -> (a, Vector Name)
runCapture stack
  = flip runState V.empty
  . flip runReaderT Env { envStack = stack, envDepth = 0 }

addName :: Name -> Capture Name
addName name = do
  names <- lift get
  case V.elemIndex name names of
    Just existing -> return $ Name existing
    Nothing -> do
      lift $ put (names <> V.singleton name)
      return . Name $ V.length names

captureTerm :: ResolvedTerm -> Capture ResolvedTerm
captureTerm typed = case typed of
  Builtin{} -> return typed
  Call{} -> return typed

  Compose terms loc -> Compose
    <$> T.mapM captureTerm terms
    <*> pure loc

  Lambda name terms loc -> let
    inside env@Env{..} = env
      { envStack = mapHead succ envStack
      , envDepth = succ envDepth
      }
    in Lambda name
      <$> local inside (captureTerm terms)
      <*> pure loc

  PairTerm a b loc -> PairTerm
    <$> captureTerm a
    <*> captureTerm b
    <*> pure loc

  Push value loc -> Push <$> captureValue value <*> pure loc

  VectorTerm items loc -> VectorTerm
    <$> T.mapM captureTerm items
    <*> pure loc

closeLocal :: Name -> Capture (Maybe Name)
closeLocal (Name index) = do
  stack <- asks envStack
  depth <- asks envDepth
  case stack of
    (here : _)
      | index >= here
      -> Just <$> addName (Name $ index - depth)
    _ -> return Nothing

captureValue :: ResolvedValue -> Capture ResolvedValue
captureValue value = case value of
  Bool{} -> return value
  Char{} -> return value
  Closed{} -> return value
  Closure names term x -> Closure
    <$> T.mapM close names
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

  Float{} -> return value
  Function terms x -> let
    inside env@Env{..} = env { envStack = 0 : envStack }
    in Function <$> local inside (captureTerm terms) <*> pure x
  Int{} -> return value

  Local name x -> do
    closed <- closeLocal name
    return $ case closed of
      Nothing -> value
      Just closedName -> Closed closedName x

  Unit{} -> return value
  String{} -> return value
