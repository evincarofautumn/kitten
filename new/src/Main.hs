{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kitten (compile, runKitten)
import Kitten.Amd64 (amd64)
import Kitten.Name (GeneralName(..), Qualified(..))
import Report
import System.Environment
import System.Exit
import System.IO
import qualified Data.Text as Text
import qualified Interact
import qualified Kitten.Platform as Platform
import qualified Kitten.Vocabulary as Vocabulary

main :: IO ()
main = do
  hSetEncoding stdout utf8
  paths <- getArgs
  case paths of
    [] -> Interact.run
    _ -> runBatch paths

runBatch :: [FilePath] -> IO ()
runBatch paths = do
  result <- runKitten
    $ compile
    [QualifiedName $ Qualified Vocabulary.global "IO"]
    Nothing
    -- FIXME: Use proper library path lookup.
    ("common.ktn" : paths)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> putStrLn $ Text.unpack $ amd64 program Platform.OSX
