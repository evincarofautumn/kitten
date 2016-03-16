{-# LANGUAGE OverloadedStrings #-}

module Interact
  ( run
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Kitten (runKitten)
import Kitten.Informer (checkpoint)
import Kitten.Name (GeneralName(..), Qualified(..))
import Report
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.Printf (printf)
import qualified Data.Text.IO as Text
import qualified Kitten as Kitten
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

run :: IO ()
run = do
  commonDictionary <- runKitten $ do
    fragment <- Kitten.fragmentFromSource
      [QualifiedName $ Qualified Vocabulary.global "IO"]
        1 "<interactive>" commonSource
    Enter.fragment fragment Dictionary.empty
  dictionaryRef <- newIORef =<< case commonDictionary of
    Left reports -> do
      reportAll reports
      exitFailure
    Right result -> return result
  lineNumberRef <- newIORef (1 :: Int)
  putStrLn "Welcome to Kitten! Type //help for help or //quit to quit."
  let
    loop = do
      lineNumber <- readIORef lineNumberRef
      printf "% 4d: " lineNumber
      hFlush stdout
      mLine <- try Text.getLine
      case mLine of
        Left e -> if isEOFError e then putStrLn "" >> bye else ioError e
        Right line -> case line of
          "//dict" -> do
            dictionary <- readIORef dictionaryRef
            liftIO $ mapM_ (putStrLn . Pretty.render . pPrint . fst)
              $ Dictionary.toList dictionary
            loop
          "//quit" -> bye
          _ -> do
            dictionary <- readIORef dictionaryRef
            mDictionary' <- runKitten $ do
              fragment <- Kitten.fragmentFromSource
                [QualifiedName $ Qualified Vocabulary.global "IO"]
                lineNumber "<interactive>" line
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
                modifyIORef' lineNumberRef (+ 1)
                loop
  loop
  where
  bye = do
    liftIO $ putStrLn "Bye!"
    return ()

commonSource :: Text
commonSource = "\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E):\n\
\  with (+IO)\n\
\"
