{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Kitten (compile, runKitten)
import Kitten.Interpret (interpret)
import Kitten.Name (GeneralName(..), Qualified(..))
import Report
import System.Environment
import System.Exit
import System.IO
import qualified Interact
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
    $ compile mainPermissions Nothing
    -- FIXME: Use proper library path lookup.
    ("common.ktn" : paths)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> void $ interpret program
      Nothing [] stdin stdout stderr []
  where
    mainPermissions =
      [ QualifiedName $ Qualified Vocabulary.global "IO"
      , QualifiedName $ Qualified Vocabulary.global "Fail"
      ]
