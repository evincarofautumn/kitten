{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Kitten (compile, runKitten)
import Kitten.Name (GeneralName(..), Qualified(..))
import Report
import System.Environment
import System.Exit
import System.IO
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Interact
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

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
    $ compile [QualifiedName $ Qualified Vocabulary.global "IO"] paths
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> putStrLn $ Pretty.render $ pPrint program
