{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Kitten (compile, runKitten)
import Kitten.Informer (checkpoint)
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Report (Report)
import System.Environment
import System.Exit
import System.IO
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text.IO as Text
import qualified Kitten as Kitten
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Report as Report
import qualified Kitten.Vocabulary as Vocabulary
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
  result <- runKitten $ compile [QualifiedName $ Qualified Vocabulary.global "IO"] paths
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    -- Pretty.render $ pPrint
    Right program -> putStrLn $ show program

runInteractive :: IO ()
runInteractive = do
  commonDictionary <- runKitten $ do
    fragment <- Kitten.fragmentFromSource
      [QualifiedName $ Qualified Vocabulary.global "IO"]
        "<interactive>" commonSource
    Enter.fragment fragment Dictionary.empty
  dictionaryRef <- newIORef =<< case commonDictionary of
    Left reports -> do
      reportAll reports
      exitFailure
    Right result -> return result
  putStrLn "Welcome to Kitten! Type //help for help or //quit to quit."
  let
    loop = do
      putStr ">>> "
      hFlush stdout
      line <- Text.getLine
      case line of
        "//dict" -> do
          dictionary <- readIORef dictionaryRef
          liftIO $ mapM_ (putStrLn . Pretty.render . pPrint . fst)
            $ Dictionary.toList dictionary
          loop
        "//quit" -> do
          liftIO $ putStrLn "Bye!"
          return ()
        _ -> do
          dictionary <- readIORef dictionaryRef
          mDictionary' <- runKitten $ do
            fragment <- Kitten.fragmentFromSource
              [QualifiedName $ Qualified Vocabulary.global "IO"]
              "<interactive>" line
            dictionary' <- Enter.fragment fragment dictionary
            checkpoint
            return dictionary'
          case mDictionary' of
            Left reports -> do
              reportAll reports
              loop
            Right dictionary' -> do
              putStrLn "Okay."
              writeIORef dictionaryRef dictionary'
              loop
  loop

reportAll :: [Report] -> IO ()
reportAll = mapM_ $ hPutStrLn stderr . Pretty.render . Report.human

commonSource :: Text
commonSource = "\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E):\n\
\  with (+IO)\n\
\"
