{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Kitten (compile, runKitten)
import Kitten.Report (Report)
import System.Environment
import System.Exit
import System.IO
import Text.Parsec.Text ()
import qualified Data.Text.IO as Text
import qualified Kitten as Kitten
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Report as Report
import qualified Text.PrettyPrint as Pretty

main :: IO ()
main = do
  hSetEncoding stdout utf8
  paths <- getArgs
  case paths of
    [] -> runInteractive
    _ -> runBatch paths

runBatch :: [FilePath] -> IO ()
runBatch paths = do
  result <- runKitten $ compile paths
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    -- Pretty.render $ pPrint
    Right program -> putStrLn $ show program

runInteractive :: IO ()
runInteractive = do
  dictionaryRef <- newIORef Dictionary.empty
  let
    loop = do
      putStr ">>> "
      hFlush stdout
      line <- Text.getLine
      case line of
        "quit" -> do
          liftIO $ putStrLn "bye"
          return ()
        _ -> do
          dictionary <- readIORef dictionaryRef
          mDictionary' <- runKitten $ do
            fragment <- Kitten.fragmentFromSource "<interactive>" line
            Enter.fragment fragment dictionary
          case mDictionary' of
            Left reports -> do
              reportAll reports
              loop
            Right dictionary' -> do
              writeIORef dictionaryRef dictionary'
              loop
  loop

reportAll :: [Report] -> IO ()
reportAll = mapM_ $ hPutStrLn stderr . Pretty.render . Report.human
