{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Kitten.Compile
import Kitten.Fragment
import Kitten.Interpret

data Arguments = Arguments
  { enableImplicitPrelude :: Bool
  , entryPoints :: [FilePath]
  , libraryDirectories :: [FilePath]
  , showHelp :: Bool
  , showVersion :: Bool
  }

main :: IO ()
main = do

  arguments <- parseArguments

  preludes <- filterM doesFileExist
    $ map (</> "prelude.ktn") (libraryDirectories arguments)

  prelude <- case preludes of

    [] -> do
      when (enableImplicitPrelude arguments) $ do
        hPutStrLn stderr "Missing prelude."
        exitFailure
      return []

    [filename] -> do
      source <- readFile filename
      let mPrelude = compile [] [] filename source
      Fragment{..} <- case mPrelude of
        Left compileError -> do
          hPrint stderr $ show compileError
          exitFailure
        Right prelude -> return prelude
      unless (null fragmentTerms) $ do
        hPutStrLn stderr "Prelude includes executable code."
        exitFailure
      return fragmentDefs

    _ -> do
      hPutStrLn stderr "Too many preludes."
      exitFailure

  forM_ (entryPoints arguments) $ \ filename -> do
    program <- readFile filename
    case compile [] prelude filename program of
      Left compileError -> print compileError
      Right resolved -> interpret [] prelude resolved

parseArguments :: IO Arguments
parseArguments = do
  arguments <- processArgs argumentsMode

  when (showVersion arguments) $ do
    putStrLn "Kitten version 1.0"
    exitSuccess

  when (showHelp arguments) $ do
    print $ helpText [] HelpFormatDefault argumentsMode
    exitSuccess

  return arguments

argumentsMode :: Mode Arguments
argumentsMode = mode "kitten" defaultArguments
  "Interprets Kitten code." bareArgument options
  where

  defaultArguments :: Arguments
  defaultArguments = Arguments
    { enableImplicitPrelude = True
    , entryPoints = []
    , libraryDirectories = []
    , showHelp = False
    , showVersion = False
    }

  bareArgument :: Arg Arguments
  bareArgument = flagArg entryPointArgument "entry-point"

  entryPointArgument
    :: FilePath -> Arguments -> Either e Arguments
  entryPointArgument path acc = Right
    $ acc { entryPoints = path : entryPoints acc }

  flagReq'
    :: [Name]
    -> FlagHelp
    -> Help
    -> Update a
    -> Flag a
  flagReq' names sample description option
    = flagReq names option sample description

  flagBool'
    :: [Name]
    -> Help
    -> (Bool -> a -> a)
    -> Flag a
  flagBool' names description option
    = flagBool names option description

  options :: [Flag Arguments]
  options =
    [ flagReq' ["L", "library"] "DIR"
      "Add library search directory."
      $ \ path acc@Arguments{..} -> Right $ acc
      { libraryDirectories = path : libraryDirectories }

    , flagBool' ["no-implicit-prelude"]
      "Disable implicit inclusion of prelude."
      $ \ flag acc@Arguments{..} -> acc
      { enableImplicitPrelude = not flag }

    , flagHelpSimple $ \ acc -> acc { showHelp = True }
    , flagVersion $ \ acc -> acc { showVersion = True }
    ]
