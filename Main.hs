{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Data.Function
import System.IO

import Term
import Token

import qualified Text.Parsec as P

main :: IO ()
main = fix $ \ loop -> do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      putStrLn $ case compile "STDIN" line of
        Left compileError -> show compileError
        Right compileResult -> show compileResult
      loop

data CompileError
  = CompileError String

instance Show CompileError where
  show (CompileError message) = message

compile :: String -> String -> Either CompileError Program
compile name source
  = case tokenize name source of
    Left error -> Left $ CompileError (show error)
    Right tokens -> case parse name tokens of
      Left error -> Left $ CompileError (show error)
      Right program -> Right program
