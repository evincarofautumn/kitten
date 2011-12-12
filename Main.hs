module Main where

import qualified Kitten
import qualified System (getArgs)

main :: IO ()
main = do
  args <- System.getArgs
  case length args of
    1 -> do
      file <- readFile . head $ args
      case Kitten.compile file of
        Left compileError ->
          putStrLn $ show compileError
        Right compileResult -> do
          putStrLn $ compileResult
    _ -> do
      putStrLn "Usage: kitten FILENAME\n"
