{-|
Module      : Kitten.Scope
Description : Scope resolution
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Scope
  ( scope
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.List (elemIndex)
import Kitten.Name (Closed(..), ClosureIndex(..), GeneralName(..), LocalIndex(..))
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))

import Debug.Trace
import qualified Text.PrettyPrint as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty(..))

-- | Whereas name resolution is concerned with resolving references to
-- definitions, scope resolution resolves local names to relative (De Bruijn)
-- indices, and converts 'Quotation's to explicit 'Capture's.

scope :: Term () -> Term ()
scope = scopeTerm [0]
  where

  scopeTerm :: [Int] -> Term () -> Term ()
  scopeTerm stack = recur
    where

    recur :: Term () -> Term ()
    recur term = case term of
      Coercion{} -> term
      Compose _ a b -> Compose () (recur a) (recur b)
      Generic{} -> error
        "generic expression should not appear before scope resolution"
      Lambda _ name _ a origin -> Lambda () name ()
        (scopeTerm (mapHead succ stack) a) origin
      Match hint _ cases else_ origin -> Match hint ()
        (map (\ (Case name a caseOrigin)
          -> Case name (recur a) caseOrigin) cases)
        ((\ (Else a elseOrigin)
          -> Else (recur a) elseOrigin) else_)
        origin
      New{} -> term
      NewClosure{} -> term
      NewVector{} -> term
      Push _ value origin -> Push () (scopeValue stack value) origin
      Word _ _ (LocalName index) _ origin
        -> Push () (scopeValue stack (Local index)) origin
      Word{} -> term

  scopeValue :: [Int] -> Value () -> Value ()
  scopeValue stack value = case value of
    Algebraic{} -> value
    Array{} -> error "array should not appear before runtime"
    Capture{} -> error "capture should not appear before scope resolution"
    Character{} -> value
    Closed{} -> error "closed name should not appear before scope resolution"
    Closure{} -> error "closure should not appear before runtime"
    Float{} -> value
    Integer{} -> value
    Local{} -> value
    Name{} -> value
    Quotation body -> Capture (map ClosedLocal capturedNames) capturedTerm
      where

      capturedTerm :: Term ()
      capturedNames :: [LocalIndex]
      (capturedTerm, capturedNames) = runCapture stack' $ captureTerm scoped

      scoped :: Term ()
      scoped = scopeTerm stack' body

      stack' :: [Int]
      stack' = 0 : stack

    Text{} -> value

data ScopeEnv = ScopeEnv
  { scopeStack :: [ScopeDepth]
  , scopeDepth :: !ScopeDepth
  }

type ScopeDepth = Int

type Captured a = ReaderT ScopeEnv (State [LocalIndex]) a

runCapture :: [Int] -> Captured a -> (a, [LocalIndex])
runCapture stack = flip runState []
  . flip runReaderT ScopeEnv { scopeStack = stack, scopeDepth = 0 }

captureTerm :: Term () -> Captured (Term ())
captureTerm term = case term of
  Coercion{} -> return term
  Compose _ a b -> Compose () <$> captureTerm a <*> captureTerm b
  Generic{} -> error
    "generic expression should not appear before scope resolution"
  Lambda _ name _ a origin -> let
    inside env = env
      { scopeStack = mapHead succ (scopeStack env)
      , scopeDepth = succ (scopeDepth env)
      }
    in Lambda () name ()
      <$> local inside (captureTerm a) <*> pure origin
  Match hint _ cases else_ origin -> Match hint ()
    <$> mapM captureCase cases <*> captureElse else_ <*> pure origin
    where

    captureCase :: Case () -> Captured (Case ())
    captureCase (Case name a caseOrigin)
      = Case name <$> captureTerm a <*> pure caseOrigin

    captureElse :: Else () -> Captured (Else ())
    captureElse (Else a elseOrigin)
      = Else <$> captureTerm a <*> pure elseOrigin

  New{} -> return term
  NewClosure{} -> return term
  NewVector{} -> return term
  Push _ value origin -> Push () <$> captureValue value <*> pure origin
  Word{} -> return term

captureValue :: Value () -> Captured (Value ())
captureValue value = case value of
  Algebraic{} -> error "adt should not appear before runtime"
  Array{} -> error "array should not appear before runtime"
  Capture names term -> Capture <$> mapM close names <*> pure term
    where

    close :: Closed -> Captured Closed
    close original = case original of
      ClosedLocal index -> do
        closed <- closeLocal index
        return $ case closed of
          Nothing -> original
          Just index' -> ClosedClosure index'
      ClosedClosure{} -> return original
  Character{} -> return value
  Closed{} -> return value
  Closure{} -> error "closure should not appear before runtime"
  Float{} -> return value
  Integer{} -> return value
  Local index -> do
    closed <- closeLocal index
    return $ case closed of
      Nothing -> value
      Just index' -> Closed index'
  Name{} -> return value
  Quotation term -> let
    inside env = env { scopeStack = 0 : scopeStack env }
    in Quotation <$> local inside (captureTerm term)
  Text{} -> return value

closeLocal :: LocalIndex -> Captured (Maybe ClosureIndex)
closeLocal (LocalIndex index) = do
  stack <- asks scopeStack
  depth <- asks scopeDepth
  case stack of
    here : _
      | index >= here
      -> fmap Just $ addName $ LocalIndex $ index - depth
    _ -> return Nothing
  where

  addName :: LocalIndex -> Captured ClosureIndex
  addName name = do
    names <- lift get
    case elemIndex name names of
      Just existing -> return $ ClosureIndex existing
      Nothing -> do
        lift $ put $ names ++ [name]
        return $ ClosureIndex $ length names

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
