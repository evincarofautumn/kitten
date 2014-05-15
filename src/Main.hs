{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Text (Text)
import System.Exit
import System.IO

import qualified Data.Vector as V

import Kitten.C
import Kitten.Compile
import Kitten.Error
import Kitten.Interpret
import Kitten.IR
import Kitten.Types
import Kitten.Util.Monad

import qualified Kitten.Util.Text as T

import Arguments
import Interactive

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  let
    defaultConfig filename program = Config
      { configDumpResolved = argsDumpResolved arguments
      , configDumpScoped = argsDumpScoped arguments
      , configEnforceBottom = True
      , configFirstLine = 1
      , configImplicitPrelude = argsEnableImplicitPrelude arguments
      , configLibraryDirectories = argsLibraryDirectories arguments
      , configName = filename
      , configOptimizations = defaultOptimizations
      , configPredefined = V.empty
      , configSource = program
      , configStackTypes = V.empty
      }

  case argsEntryPoints arguments of
    [] -> runInteraction (argsEnableImplicitPrelude arguments)
    entryPoints -> interpretAll entryPoints arguments defaultConfig

interpretAll
  :: [FilePath]
  -> Arguments
  -> (FilePath -> Text -> Config)
  -> IO ()
interpretAll entryPoints Arguments{..} config
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
      Right (result, ip, _type) -> do
        out <- maybe (return stdout) (flip openFile WriteMode) argsOutputPath
        case argsCompileMode of
          CheckMode -> noop
          CompileMode OutputIr -> V.mapM_ (hPrint out)
            $ flattenedBlock (flattenProgram result)
          CompileMode OutputC -> V.mapM_ (hPutStrLn out . T.unpack)
            $ toC $ flattenProgram result
          InterpretMode -> void $ interpret (Just ip) [] result
