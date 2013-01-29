module Kitten.Repl
  ( runRepl
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Vector (Vector)
import System.IO

import Kitten.Compile
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Prelude
import Kitten.Resolve

runRepl :: IO ()
runRepl = evalStateT repl empty

data Repl = Repl
  { replStack :: [Value]
  , replDefs :: Vector (Def Resolved)
  }

empty :: Repl
empty = Repl [] prelude

repl :: StateT Repl IO ()
repl = do
  line <- lift prompt
  case line of
    "" -> repl
    ":quit" -> quit
    ":q" -> quit
    ":clear" -> clear
    ":c" -> clear
    (':' : expression) -> do
      stack <- gets replStack
      lift $ case typecheck stack prelude replName expression of
        Left compileError -> print compileError
        Right type_ -> print type_
      repl
    _ -> do
      defs <- gets replDefs
      stack <- gets replStack
      case compile stack defs replName line of
        Left compileError -> lift $ print compileError
        Right compileResult -> do
          stack' <- lift $ interpret stack compileResult
          lift . putStrLn . unwords . reverse $ map show stack'
          modify $ \ s -> s
            { replStack = stack'
            , replDefs = fragmentDefs compileResult
            }
      repl
  where
  prompt = do
    putStr "> "
    hFlush stdout
    getLine
  quit = return ()
  clear = put empty >> repl
  replName = "REPL"
