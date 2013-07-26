{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret.Monad
  ( Env(..)
  , Interpret
  , InterpretM
  , getClosed
  , getLocal
  , here
  , popData
  , popLocal
  , pushData
  , pushLocal
  , withClosure
  , withLocation
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Exit
import System.IO

import Kitten.Def
import Kitten.Location
import Kitten.Name
import Kitten.Resolved

type Interpret = InterpretM ()

type InterpretM a = StateT Env IO a

data Env = Env
  { envData :: [Value]
  , envLocals :: [Value]
  , envDefs :: [Def Value]
  , envClosure :: [Value]
  , envLocations :: [Location]
  }

getClosed :: Name -> InterpretM Value
getClosed (Name index) = do
  closure <- gets envClosure
  return $ closure !! index

getLocal :: Name -> InterpretM Value
getLocal (Name index) = do
  locals <- gets envLocals
  return $ locals !! index

here :: InterpretM Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

popData :: InterpretM Value
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> do
      loc <- here
      lift $ do
        hPutStrLn stderr $ show loc ++ ": stack underflow"
        exitFailure
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popLocal :: Interpret
popLocal = do
  localStack <- gets envLocals
  case localStack of
    [] -> do
      loc <- here
      lift $ do
        hPutStrLn stderr $ show loc ++ ": local stack underflow"
        exitFailure
    (_ : down) -> modify $ \ env -> env { envLocals = down }

pushData :: Value -> Interpret
pushData value = modify $ \ env@Env{..}
  -> env { envData = value : envData }

pushLocal :: Value -> Interpret
pushLocal value = modify $ \ env@Env{..}
  -> env { envLocals = value : envLocals }

withClosure :: [Value] -> InterpretM a -> InterpretM a
withClosure values action = do
  closure <- gets envClosure
  modify $ \ env -> env { envClosure = values }
  result <- action
  modify $ \ env -> env { envClosure = closure }
  return result

withLocation :: Location -> InterpretM a -> InterpretM a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
