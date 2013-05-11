module Main where

import Control.Monad
import System.Environment

import Kitten.Compile
import Kitten.Interpret
import Kitten.Prelude

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \ filename -> do
    program <- readFile filename
    mPrelude <- compilePrelude
    prelude <- case mPrelude of
      Left compileError -> fail $ show compileError
      Right prelude -> return prelude
    case compile [] prelude filename program of
      Left compileError -> print compileError
      Right resolved -> interpret [] prelude resolved
