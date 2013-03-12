module Main where

import Control.Monad
import Data.Monoid
import System.Environment

import Kitten.Compile
import Kitten.Interpret
import Kitten.Prelude
import Kitten.Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    filenames -> forM_ filenames $ \ filename -> do
      program <- readFile filename
      case compile [] prelude mempty filename program of
        Left compileError -> print compileError
        Right resolved -> void $ interpret [] resolved
