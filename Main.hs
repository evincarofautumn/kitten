module Main where

import qualified Kitten
import qualified System (getArgs)
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- System.getArgs
  case length args of
    1 -> do
      file <- readFile . head $ args
      case Kitten.compile file of
        Left compileError ->
          die $ show compileError
        Right compileResult -> do
          putStrLn $ compileResult
    _ -> do
      die "Usage: kitten FILENAME\n"

die :: String -> IO ()
die msg = do
  hPutStrLn stderr msg
  exitFailure