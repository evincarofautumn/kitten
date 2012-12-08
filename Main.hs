{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import System.IO

import Error
import Resolve
import Term

import qualified Token

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

compile :: String -> String -> Either CompileError (Program Resolved)
compile name
  = failIfError . Token.tokenize name
  >=> failIfError . parse name
  >=> resolveProgram
  where
  failIfError = mapLeft (CompileError . show)

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
