{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

import Kitten (compile, runKitten)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  paths <- getArgs
  result <- runKitten $ compile paths
  case result of
    Left reports -> do
      forM_ reports $ mapM_ (hPutStrLn stderr . show) . reverse
      exitFailure
    Right program -> putStrLn $ Pretty.render $ pPrint program
