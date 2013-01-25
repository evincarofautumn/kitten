module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Resolve (Resolved)
import Kitten.Type (Type)

import qualified Kitten.Resolve as Resolve
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
import qualified Kitten.Token as Token

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> evalStateT repl prelude
    filenames -> forM_ filenames $ \ filename -> do
      program <- readFile filename
      case compile prelude replName program of
        Left compileError -> print compileError
        Right resolved -> interpret resolved
  where
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
  prompt = do
    putStr "> "
    hFlush stdout
    getLine
  quit = return ()
  clear = put [] >> repl
  replName = "REPL"
  prelude = []

compile
  :: [Def Resolved]
  -> String
  -> String
  -> Either CompileError (Fragment Resolved)
compile prelude name source = do
  tokenized <- failIfError $ Token.tokenize name source
  parsed <- failIfError $ Term.parse name tokenized
  resolved <- Resolve.resolveFragment prelude parsed
  void $ Type.typeFragment resolved
  return resolved

typecheck
  :: [Def Resolved]
  -> String
  -> String
  -> Either CompileError Type
typecheck prelude name
  = failIfError . Token.tokenize name
  >=> failIfError . Term.parse name
  >=> Resolve.resolveFragment prelude
  >=> liftM (Type.manifestType . fragmentTerm) . Type.typeFragment
