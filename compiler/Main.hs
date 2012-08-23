module Main where

import Kitten
import qualified Text

import System.Environment
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      let filename = head args
      file <- readFile filename
      case compile filename file of
        Left compileError   -> die compileError
        Right compileResult -> putStrLn $ Text.unpack compileResult
    _ ->
      die "Usage: kitten FILENAME\n"

die
  :: (Show a)
  => a
  -> IO ()
die msg = do
  hPutStr stderr $ show msg
  exitFailure
