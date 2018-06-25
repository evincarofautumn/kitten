{-# LANGUAGE OverloadedStrings #-}

module Main where

import Arguments (Arguments, parseArguments)
import Control.Monad (void)
import Kitten (compile, runKitten)
import Kitten.Interpret (interpret)
import Kitten.Name (GeneralName(..), Qualified(..))
import Paths_Kitten
import Report
import System.Exit
import System.IO
import qualified Arguments
import qualified Interact
import qualified Kitten.Vocabulary as Vocabulary

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  case Arguments.inputPaths arguments of
    [] -> case Arguments.compileMode arguments of
      Arguments.CheckMode -> do
        hPutStrLn stderr "Cannot run interactively in check mode."
        exitFailure
      Arguments.CompileMode{} -> do
        hPutStrLn stderr "Cannot run interactively in compile mode."
        exitFailure
      Arguments.InterpretMode -> Interact.run
    _ -> runBatch arguments

runBatch :: Arguments -> IO ()
runBatch arguments = do
  let paths = Arguments.inputPaths arguments
  commonPath <- getDataFileName "common.ktn"
  result <- runKitten $ compile mainPermissions Nothing (commonPath : paths)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> case Arguments.compileMode arguments of
      Arguments.CheckMode -> return ()
      Arguments.CompileMode _format -> return ()
      Arguments.InterpretMode -> void $ interpret program
        Nothing [] stdin stdout stderr []
  where
    mainPermissions =
      [ QualifiedName $ Qualified Vocabulary.global "IO"
      , QualifiedName $ Qualified Vocabulary.global "Fail"
      ]
