module Main where

import Control.Monad
import System.Environment

import Kitten.Compile
import Kitten.Interpret

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \ filename -> do
    program <- readFile filename
    case compile [] [] filename program of
      Left compileError -> print compileError
      Right resolved -> interpret [] resolved
