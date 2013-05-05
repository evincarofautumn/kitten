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
import Kitten.Location
import Kitten.Name
import Kitten.Resolve
import Kitten.Util.List

scope :: Fragment Resolved -> Fragment Resolved
scope fragment@Fragment{..} = fragment
  { fragmentDefs = map scopeDef fragmentDefs
  , fragmentTerms = map (scopeTerm [0]) fragmentTerms
  }

scopeDef :: Def Resolved -> Def Resolved
scopeDef def@Def{..} = def
  { defTerm = scopeTerm [0] defTerm }

scopeTerm :: [Int] -> Resolved -> Resolved
scopeTerm stack resolved = case resolved of
  Block terms -> Block $ map (scopeTerm stack) terms
  Builtin{} -> resolved
  Closed{} -> resolved
  If condition true false loc -> If
    (map (scopeTerm stack) condition)
    (map (scopeTerm stack) true)
    (map (scopeTerm stack) false)
    loc
  Local{} -> resolved
  Push value loc -> Push (scopeValue stack value) loc
  Scoped terms loc -> Scoped
    (map (scopeTerm (mapHead succ stack)) terms)
    loc

scopeValue :: [Int] -> Value -> Value
scopeValue stack value = case value of

  Activation{} -> value

  Bool{} -> value

  Closure{} -> value

  Escape{} -> value

  Float{} -> value

  Function _ funTerms
    -> Closure capturedNames rescopedTerms
    where

    rescopedTerms :: [Resolved]
    rescopedTerms = foldr go capturedTerms
      [0 .. pred $ length capturedNames]
      where
        go :: Int -> [Resolved] -> [Resolved]
        go index terms =
          [ Closed (Name index) GeneratedLocation
          , Scoped terms GeneratedLocation
          ]

    capturedTerms :: [Resolved]
    capturedNames :: [Name]
    (capturedTerms, capturedNames)
      = runCapture stack'
      $ mapM captureTerm scopedTerms

    scopedTerms :: [Resolved]
    scopedTerms = map (scopeTerm stack') funTerms

    stack' :: [Int]
    stack' = 0 : stack

  Int{} -> value

  Text{} -> value

  Vector anno values -> Vector anno
    $ map (scopeValue stack) values

  Word{} -> value

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
    Block terms -> Block <$> mapM captureTerm terms
    Builtin{} -> return resolved
    Closed{} -> return resolved
    If condition true false loc -> If
      <$> mapM captureTerm condition
      <*> mapM captureTerm true
      <*> mapM captureTerm false
      <*> pure loc
    Local name loc -> do
      closed <- closeLocal name
      return $ case closed of
        Nothing -> resolved
        Just closedName -> Closed closedName loc
    Push value loc -> Push <$> captureValue value <*> pure loc
    Scoped terms loc -> let
        inside env@Env{..} = env
          { envStack = mapHead succ envStack
          , envDepth = succ envDepth
          }
      in Scoped
        <$> local inside (mapM captureTerm terms)
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
captureValue value = do
  case value of
    Activation{} -> return value
    Bool{} -> return value
    Closure names _ -> do
      mapM_ closeLocal names
      return value
    Escape{} -> return value
    Float{} -> return value
    Function anno terms -> let
        inside env@Env{..} = env { envStack = 0 : envStack }
      in Function anno
         <$> local inside (mapM captureTerm terms)
    Int{} -> return value
    Text{} -> return value
    Vector anno values -> Vector anno
      <$> mapM captureValue values
    Word{} -> return value
