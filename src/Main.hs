{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

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
  libraryDirectories <- splitSearchPath <$> catch (getEnv "KITTEN_LIB")
    (\e -> if isDoesNotExistError e then return [] else throwIO e)
  let
    defaultConfig = Config
      { configDumpResolved = argsDumpResolved arguments
      , configDumpScoped = argsDumpScoped arguments
      , configEnforceBottom = True
      , configFirstLine = 1
      , configImplicitPrelude = argsEnableImplicitPrelude arguments
      , configLibraryDirectories
        = argsLibraryDirectories arguments ++ libraryDirectories
      , configName = ""
      , configOptimizations = defaultOptimizations
      , configPredefined = V.empty
      , configSource = ""
      , configStackTypes = V.empty
      }

  case argsEntryPoints arguments of
    [] -> runInteraction defaultConfig
    entryPoints -> interpretAll entryPoints arguments defaultConfig

interpretAll
  :: [FilePath]
  -> Arguments
  -> Config
  -> IO ()
interpretAll entryPoints Arguments{..} config
  = mapM_ interpretOne entryPoints
  where
  interpretOne :: FilePath -> IO ()
  interpretOne filename = do
    source <- T.readFileUtf8 filename
    mResult <- compile config
      { configName = filename
      , configSource = source
      } emptyProgram
    case mResult of
      Left compileErrors -> do
        printCompileErrors compileErrors
        exitFailure
      Right (result, ip, _type) -> do
        out <- maybe (return stdout) (`openFile` WriteMode) argsOutputPath
        case argsCompileMode of
          CheckMode -> noop
          CompileMode OutputIr -> V.mapM_ (hPrint out)
            $ flattenedBlock (flattenProgram result)
          CompileMode OutputC -> V.mapM_ (hPutStrLn out . T.unpack)
            $ toC $ flattenProgram result
          InterpretMode -> void $ interpret (Just ip) [] result
