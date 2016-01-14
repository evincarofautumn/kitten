{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Kitten (compile, runKitten)
import System.Environment
import System.Exit
import System.IO
import Text.Parsec.Text ()
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Report as Report
import qualified Text.PrettyPrint as Pretty

main :: IO ()
main = do
  hSetEncoding stdout utf8
  paths <- getArgs
  result <- runKitten $ compile paths
  case result of
    Left reports -> do
      mapM_ (hPutStrLn stderr . Pretty.render . Report.human) reports
      exitFailure
    Right program -> putStrLn $ Pretty.render $ pPrint program
