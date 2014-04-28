{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Text (Text)
import System.Exit
import System.IO

import qualified Data.Vector as V

import Kitten.Compile
import Kitten.Error
import Kitten.Interpret
import Kitten.IR
import Kitten.Util.Monad

import qualified Kitten.Compile as Compile
import qualified Kitten.Util.Text as T

import Arguments
import Interactive

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  let
    defaultConfig filename program = Compile.Config
      { Compile.dumpResolved = argsDumpResolved arguments
      , Compile.dumpScoped = argsDumpScoped arguments
      , Compile.firstLine = 1
      , Compile.implicitPrelude = argsEnableImplicitPrelude arguments
      , Compile.libraryDirectories = argsLibraryDirectories arguments
      , Compile.name = filename
      , Compile.predefined = V.empty
      , Compile.source = program
      , Compile.stackTypes = V.empty
      }

  case argsEntryPoints arguments of
    [] -> runInteraction
    entryPoints -> interpretAll entryPoints
      (argsCompileMode arguments) defaultConfig

interpretAll
  :: [FilePath]
  -> CompileMode
  -> (FilePath -> Text -> Compile.Config)
  -> IO ()
interpretAll entryPoints compileMode config
  = mapM_ interpretOne entryPoints
  where
  interpretOne :: FilePath -> IO ()
  interpretOne filename = do
    source <- T.readFileUtf8 filename
    mResult <- compile (config filename source) emptyProgram
    case mResult of
      Left compileErrors -> do
        printCompileErrors compileErrors
        exitFailure
      Right (result, ip, _type) -> case compileMode of
        CheckMode -> noop
        CompileMode -> V.mapM_ print . flattenedBlock $ flattenProgram result
        InterpretMode -> void $ interpret (Just ip) [] result
