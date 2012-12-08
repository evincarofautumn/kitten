{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
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
compile name
  = failIfError . tokenize name
  >=> failIfError . parse name
  where failIfError = mapLeft (CompileError . show)

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
