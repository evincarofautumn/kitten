module Kitten.Repl
  ( runRepl
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.IO

import Kitten.Compile
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Prelude
import Kitten.Resolve

runRepl :: IO ()
runRepl = evalStateT repl prelude

repl :: StateT [Def Resolved] IO ()
repl = do
  line <- lift prompt
  case line of
    "" -> repl
    ":quit" -> quit
    ":q" -> quit
    ":clear" -> clear
    ":c" -> clear
    (':' : expression) -> do
      lift $ case typecheck prelude replName expression of
        Left compileError -> print compileError
        Right type_ -> print type_
      repl
    _ -> do
      defs <- get
      case compile defs replName line of
        Left compileError -> lift $ print compileError
        Right compileResult -> do
          lift $ interpret compileResult
          put $ defs ++ fragmentDefs compileResult
      repl
  where
  prompt = do
    putStr "> "
    hFlush stdout
    getLine
  quit = return ()
  clear = put [] >> repl
  replName = "REPL"
