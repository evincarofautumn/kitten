{-# LANGUAGE RecordWildCards #-}

module Repl
  ( runRepl
  ) where

import Control.Applicative hiding (empty)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid
import System.Console.Haskeline

import Kitten.Compile (compile)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Resolved

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Compile as Compile

data Repl = Repl
  { replStack :: [Value]
  , replDefs :: [Def Value]
  }

type ReplReader = ReaderT [Def Value] IO
type ReplState = StateT Repl ReplReader
type ReplInput = InputT ReplState

runRepl :: Fragment Value Resolved -> IO ()
runRepl prelude
  = flip runReaderT preludeDefs
  . flip evalStateT empty
  $ runInputT settings repl

  where
  empty :: Repl
  empty = Repl
    { replStack = []
    , replDefs = preludeDefs
    }

  preludeDefs :: [Def Value]
  preludeDefs = fragmentDefs prelude

repl :: ReplInput ()
repl = do
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just "" -> repl
    Just ":quit" -> quit
    Just ":q" -> quit
    Just ":clear" -> clear
    Just ":c" -> clear
    Just line -> do
      Repl{..} <- lift get
      mCompiled <- liftIO $ compile Compile.Config
        { Compile.dumpResolved = False
        , Compile.dumpScoped = False
        , Compile.libraryDirectories = []  -- TODO
        , Compile.name = replName
        , Compile.prelude = mempty { fragmentDefs = replDefs }
        , Compile.source = line
        , Compile.stack = replStack
        }
      case mCompiled of
        Left compileErrors -> liftIO
          $ printCompileErrors compileErrors
        Right compileResult -> do
          stack' <- liftIO $ interpret replStack
            mempty { fragmentDefs = replDefs }
            compileResult
          lift . modify $ \ s -> s
            { replStack = stack'
            , replDefs = replDefs <> fragmentDefs compileResult
            }
          showStack
      repl

  where
  quit :: ReplInput ()
  quit = return ()

  clear :: ReplInput ()
  clear = do
    preludeDefs <- askPreludeDefs
    lift $ put Repl
      { replStack = []
      , replDefs = preludeDefs
      }
    repl

  replName :: String
  replName = "REPL"

  liftIO :: IO a -> ReplInput a
  liftIO = lift . lift . lift

  showStack :: ReplInput ()
  showStack = do
    stack <- lift $ gets replStack
    liftIO . putStrLn . unwords . reverse $ map show stack

  askPreludeDefs :: ReplInput [Def Value]
  askPreludeDefs = lift (lift ask)

completer :: CompletionFunc ReplState
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix
  :: String
  -> StateT Repl (ReaderT [Def Value] IO) [Completion]
completePrefix prefix = do
  defs <- (++) <$> gets replDefs <*> lift ask
  let
    names = map defName defs ++ Builtin.names
    matching = filter (prefix `isPrefixOf`) names
    finished = case matching of
      [] -> True
      [_] -> True
      _ -> False
    completions = map (toCompletion finished) matching
  return completions

toCompletion :: Bool -> String -> Completion
toCompletion finished name = Completion
  { replacement = name
  , display = name
  , isFinished = finished
  }

settings :: Settings ReplState
settings = Settings
  { complete = completer
  , historyFile = Nothing
  , autoAddHistory = True
  }
