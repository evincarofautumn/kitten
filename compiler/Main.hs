module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO

import Error
import Interpret
import Program
import Resolve (Resolved)

import qualified Resolve as Resolved
import qualified Term
import qualified Type as Typed
import qualified Token

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> evalStateT repl ""
    filenames -> forM_ filenames $ \ filename -> do
      program <- readFile filename
      case compile replName program of
        Left compileError -> print compileError
        Right compileResult -> interpret compileResult
  where
  repl = do
    line <- lift $ do
      putStr "> "
      hFlush stdout
      getLine
    case line of
      "" -> repl
      ":quit" -> quit
      ":q" -> quit
      ":clear" -> clear
      ":c" -> clear
      (':' : expression) -> do
        lift $ case typecheck replName expression of
          Left compileError -> print compileError
          Right type_ -> print type_
        repl
      _ -> do
        program <- get
        let program' = program ++ '\n' : line
        case compile replName program' of
          Left compileError -> lift $ print compileError
          Right compileResult -> do
            lift $ interpret compileResult
            put program'
        repl
  quit = return ()
  clear = put [] >> repl
  replName = "REPL"

compile :: String -> String -> Either CompileError (Program Resolved)
compile name source = do
  tokenized <- failIfError $ Token.tokenize name source
  parsed <- failIfError $ Term.parse name tokenized
  resolved <- Resolved.resolveProgram parsed
  void $ Typed.typeProgram resolved
  return resolved

typecheck :: String -> String -> Either CompileError Typed.Type
typecheck name
  = failIfError . Token.tokenize name
  >=> failIfError . Term.parse name
  >=> Resolved.resolveProgram
  >=> liftM (Typed.manifestType . programTerm) . Typed.typeProgram
