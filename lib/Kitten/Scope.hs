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
import Kitten.Resolved
import Kitten.Util.List

scope :: Fragment Resolved -> Fragment Resolved
scope fragment@Fragment{..} = fragment
  { fragmentDefs = scopeDef <$> fragmentDefs
  , fragmentTerms = scopeTerm [0] <$> fragmentTerms
  }

scopeDef :: Def Resolved -> Def Resolved
scopeDef def@Def{..} = def
  { defTerm = scopeTerm [0] defTerm }

scopeTerm :: [Int] -> Resolved -> Resolved
scopeTerm stack typed = case typed of
  Builtin{} -> typed
  Call{} -> typed
  Compose terms loc -> Compose (recur <$> terms) loc
  From{} -> typed
  PairTerm as bs loc -> PairTerm (recur as) (recur bs) loc
  Push value loc -> Push (scopeValue stack value) loc
  Scoped term loc -> Scoped
    (scopeTerm (mapHead succ stack) term)
    loc
  To{} -> typed
  VectorTerm items loc -> VectorTerm (recur <$> items) loc

  where
  recur :: Resolved -> Resolved
  recur = scopeTerm stack

scopeValue :: [Int] -> Value -> Value
scopeValue stack value = case value of
  Bool{} -> value
  Char{} -> value
  Closed{} -> value
  Closure{} -> value
  Float{} -> value

  Function funTerm
    -> Closure (ClosedName <$> capturedNames) capturedTerm
    where

    capturedTerm :: Resolved
    capturedNames :: Vector Name
    (capturedTerm, capturedNames)
      = runCapture stack'
      $ captureTerm scopedTerm

    scopedTerm :: Resolved
    scopedTerm = scopeTerm stack' funTerm

    stack' :: [Int]
    stack' = 0 : stack

  Int{} -> value
  Local{} -> value
  Unit -> Unit
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

captureTerm :: Resolved -> Capture Resolved
captureTerm typed = case typed of
  Builtin{} -> return typed
  Call{} -> return typed

  Compose terms loc -> Compose
    <$> T.mapM captureTerm terms
    <*> pure loc

  From{} -> return typed

  PairTerm a b loc -> PairTerm
    <$> captureTerm a
    <*> captureTerm b
    <*> pure loc

  Push value loc -> Push <$> captureValue value <*> pure loc

  Scoped terms loc -> let
    inside env@Env{..} = env
      { envStack = mapHead succ envStack
      , envDepth = succ envDepth
      }
    in Scoped
      <$> local inside (captureTerm terms)
      <*> pure loc

  To{} -> return typed

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

captureValue :: Value -> Capture Value
captureValue value = case value of
  Bool{} -> return value
  Char{} -> return value
  Closed{} -> return value
  Closure names term -> Closure
    <$> T.mapM close names
    <*> pure term
    where
    close :: ClosedName -> Capture ClosedName
    close original@(ClosedName name) = do
      closed <- closeLocal name
      return $ case closed of
        Nothing -> original
        Just closedLocal -> ReclosedName closedLocal
    close original@(ReclosedName _) = return original

  Float{} -> return value
  Function terms -> let
    inside env@Env{..} = env { envStack = 0 : envStack }
    in Function <$> local inside (captureTerm terms)
  Int{} -> return value

  Local name -> do
    closed <- closeLocal name
    return $ case closed of
      Nothing -> value
      Just closedName -> Closed closedName

  Unit{} -> return value
  String{} -> return value
