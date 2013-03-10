module Kitten.Repl
  ( runRepl
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Vector (Vector)
import System.Console.Haskeline

import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Kitten.Compile
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Prelude
import Kitten.Resolve

import qualified Kitten.Builtin as Builtin

runRepl :: IO ()
runRepl = flip evalStateT empty $ runInputT settings repl

data Repl = Repl
  { replStack :: [Value]
  , replDefs :: !(Vector (Def Resolved))
  }

type ReplState = StateT Repl IO
type ReplInput = InputT ReplState

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
    Just (':' : expression) -> do
      Repl{..} <- lift get
      liftIO $ case typecheck replStack replDefs replName expression of
        Left compileError -> print compileError
        Right type_ -> print type_
      repl
    Just line -> do
      Repl{..} <- lift get
      case compile replStack replDefs replName line of
        Left compileError -> liftIO $ print compileError
        Right compileResult -> do
          stack' <- liftIO $ interpret replStack compileResult
          lift . modify $ \ s -> s
            { replStack = stack'
            , replDefs = fragmentDefs compileResult
            }
          showStack
      repl
  where
  quit = return ()
  clear = lift (put empty) >> repl
  replName = "REPL"
  liftIO = lift . lift
  showStack = do
    stack <- lift $ gets replStack
    liftIO . putStrLn . unwords . reverse $ map show stack

completer :: CompletionFunc ReplState
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix :: (Monad m) => String -> StateT Repl m [Completion]
completePrefix prefix = do
  defs <- liftM Vector.toList $ gets replDefs
  let
    names = map Text.unpack $ map defName defs ++ Builtin.names
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

empty :: Repl
empty = Repl [] prelude
