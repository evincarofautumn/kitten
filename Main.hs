module Main where

import qualified Kitten
import System.Environment (getArgs)
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      let filename = head args
      file <- readFile filename
      case Kitten.compile filename file of
        Left compileError ->
          die (show compileError)
        Right compileResult ->
          putStrLn compileResult
    _ ->
      die "Usage: kitten FILENAME\n"

die :: String -> IO ()
die msg = do
  hPutStr stderr msg
  exitFailure
