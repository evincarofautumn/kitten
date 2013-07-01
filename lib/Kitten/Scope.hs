{-# LANGUAGE RecordWildCards #-}

module Kitten.Scope
  ( scope
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List

import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Typed
import Kitten.Util.List

scope :: Fragment Value Typed -> Fragment Value Typed
scope fragment@Fragment{..} = fragment
  { fragmentDefs = map scopeDef fragmentDefs
  , fragmentTerms = map (scopeTerm [0]) fragmentTerms
  }

scopeDef :: Def Value -> Def Value
scopeDef def@Def{..} = def
  { defTerm = scopeValue [0] defTerm }

scopeTerm :: [Int] -> Typed -> Typed
scopeTerm stack typed = case typed of

  Call{} -> typed

  Compose terms -> Compose (map (scopeTerm stack) terms)

  Builtin{} -> typed

  If true false loc -> If
    (scopeTerm stack true)
    (scopeTerm stack false)
    loc

  Push value loc -> Push (scopeValue stack value) loc

  Scoped term loc -> Scoped
    (scopeTerm (mapHead succ stack) term)
    loc

scopeValue :: [Int] -> Value -> Value
scopeValue stack value = case value of
  Activation{} -> value
  Bool{} -> value
  Char{} -> value
  Closed{} -> value
  Closure{} -> value
  Escape{} -> value
  Float{} -> value

  Function funTerms
    -> Closure (map ClosedName capturedNames) capturedTerms
    where

    capturedTerms :: Typed
    capturedNames :: [Name]
    (capturedTerms, capturedNames)
      = runCapture stack'
      $ captureTerm scopedTerms

    scopedTerms :: Typed
    scopedTerms = scopeTerm stack' funTerms

    stack' :: [Int]
    stack' = 0 : stack

  Handle{} -> value
  Int{} -> value
  Local{} -> value
  Pair a b -> Pair (scopeValue stack a) (scopeValue stack b)
  Unit -> Unit
  Vector values -> Vector (map (scopeValue stack) values)

data Env = Env
  { envStack :: [Int]
  , envDepth :: Int
  }

type Capture a = ReaderT Env (State [Name]) a

runCapture :: [Int] -> Capture a -> (a, [Name])
runCapture stack
  = flip runState []
  . flip runReaderT Env { envStack = stack, envDepth = 0 }

addName :: Name -> Capture Name
addName name = do
  names <- lift get
  case elemIndex name names of
    Just existing -> return $ Name existing
    Nothing -> do
      lift $ put (names ++ [name])
      return . Name $ length names

captureTerm :: Typed -> Capture Typed
captureTerm typed = case typed of

  Call{} -> return typed

  Compose terms -> Compose <$> mapM captureTerm terms

  Builtin{} -> return typed

  If true false loc -> If
    <$> captureTerm true
    <*> captureTerm false
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
  Activation{} -> return value
  Bool{} -> return value
  Char{} -> return value
  Closed{} -> return value
  Closure names term -> Closure
    <$> mapM close names
    <*> pure term
    where
    close :: ClosedName -> Capture ClosedName
    close original@(ClosedName name) = do
      closed <- closeLocal name
      return $ case closed of
        Nothing -> original
        Just closedLocal -> ReclosedName closedLocal
    close original@(ReclosedName _) = return original

  Escape{} -> return value
  Float{} -> return value
  Function terms -> let
    inside env@Env{..} = env { envStack = 0 : envStack }
    in Function <$> local inside (captureTerm terms)
  Handle{} -> return value
  Int{} -> return value

  Local name -> do
    closed <- closeLocal name
    return $ case closed of
      Nothing -> value
      Just closedName -> Closed closedName

  Pair a b -> Pair <$> captureValue a <*> captureValue b
  Unit{} -> return value
  Vector values -> Vector <$> mapM captureValue values
