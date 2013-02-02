module Kitten.Repl
  ( runRepl
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Vector (Vector)
import System.Console.Haskeline

import Kitten.Compile
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Prelude
import Kitten.Resolve

runRepl :: IO ()
runRepl = runInputT defaultSettings $ evalStateT repl emptyRepl

data Repl = Repl
  { replStack :: [Value]
  , replDefs :: Vector (Def Resolved)
  }

type ReplT = StateT Repl

emptyRepl :: Repl
emptyRepl = Repl [] prelude

repl :: ReplT (InputT IO) ()
repl = do
  mLine <- lift $ getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just "" -> repl
    Just ":quit" -> quit
    Just ":q" -> quit
    Just ":clear" -> clear
    Just ":c" -> clear
    Just (':' : expression) -> do
      Repl{..} <- get
      lift . lift $ case typecheck replStack replDefs replName expression of
        Left compileError -> print compileError
        Right type_ -> print type_
      repl
    Just line -> do
      Repl{..} <- get
      case compile replStack replDefs replName line of
        Left compileError -> lift . lift $ print compileError
        Right compileResult -> do
          stack' <- lift . lift $ interpret replStack compileResult
          modify $ \ s -> s
            { replStack = stack'
            , replDefs = fragmentDefs compileResult
            }
          showStack
      repl
  where
  quit = return ()
  clear = put emptyRepl >> repl
  replName = "REPL"
  showStack = do
    stack <- gets replStack
    lift . lift . putStrLn . unwords . reverse $ map show stack
