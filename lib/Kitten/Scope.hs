{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Scope
  ( scope
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List

import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve
import Kitten.Util.List

scope :: Fragment Resolved -> Fragment Resolved
scope fragment@Fragment{..} = fragment
  { fragmentDefs = map scopeDef fragmentDefs
  , fragmentTerm = scopeTerm [0] fragmentTerm
  }

scopeDef :: Def Resolved -> Def Resolved
scopeDef def@Def{..} = def
  { defTerm = scopeTerm [0] defTerm }

scopeTerm :: [Int] -> Resolved -> Resolved
scopeTerm stack resolved = case resolved of
  Push value -> Push $ scopeValue stack value
  Builtin{} -> resolved
  Scoped terms -> Scoped
    $ map (scopeTerm (mapHead succ stack)) terms
  Local{} -> resolved
  Closed{} -> resolved
  Compose terms -> Compose $ map (scopeTerm stack) terms

scopeValue :: [Int] -> Value -> Value
scopeValue stack value = case value of
  Fun funTerms -> Closure capturedNames rescopedTerms
    where

    rescopedTerms :: [Resolved]
    rescopedTerms = foldr go capturedTerms
      [0 .. pred $ length capturedNames]
      where
        go :: Int -> [Resolved] -> [Resolved]
        go index terms = [Closed $ Name index, Scoped terms]

    capturedTerms :: [Resolved]
    capturedNames :: [Name]
    (capturedTerms, capturedNames)
      = runCapture stack'
      $ mapM captureTerm scopedTerms

    scopedTerms :: [Resolved]
    scopedTerms = map (scopeTerm stack') funTerms

    stack' :: [Int]
    stack' = 0 : stack

  Word{} -> value
  Int{} -> value
  Bool{} -> value
  Text{} -> value
  Closure{} -> value
  Closure'{} -> value
  Vec values -> Vec
    $ map (scopeValue stack) values
  Tuple values -> Tuple
    $ map (scopeValue stack) values

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

captureTerm :: Resolved -> Capture Resolved
captureTerm resolved = do
  case resolved of
    Push value -> Push <$> captureValue value
    Builtin{} -> return resolved
    Scoped terms -> Scoped
      <$> local (\ env@Env{..} -> env
        { envStack = mapHead succ envStack
        , envDepth = succ envDepth
        })
        (mapM captureTerm terms)
    Local name -> do
      closed <- closeLocal name
      return $ case closed of
        Nothing -> resolved
        Just closedName -> Closed closedName
    Closed{} -> return resolved
    Compose terms -> Compose <$> mapM captureTerm terms

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
captureValue value = do
  case value of
    Word{} -> return value
    Int{} -> return value
    Bool{} -> return value
    Text{} -> return value
    Closure names _ -> do
      mapM_ closeLocal names
      return value
    Closure' _ _ -> return value
    Vec values -> Vec <$> mapM captureValue values
    Tuple values -> Tuple <$> mapM captureValue values
    Fun terms -> Fun
      <$> local (\ env@Env{..} -> env { envStack = 0 : envStack })
        (mapM captureTerm terms)
