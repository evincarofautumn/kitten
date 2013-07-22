{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Monoid
import System.Console.CmdArgs.Explicit
import System.Exit
import System.IO

import Kitten.Compile (compile)
import Kitten.Error (CompileError)
import Kitten.Fragment
import Kitten.Imports
import Kitten.Interpret
import Kitten.Yarn (yarn)

import qualified Kitten.Compile as Compile

data CompileMode
  = CompileMode
  | InterpretMode

data Arguments = Arguments
  { compileMode :: CompileMode
  , dumpResolved :: Bool
  , dumpScoped :: Bool
  , enableImplicitPrelude :: Bool
  , entryPoints :: [FilePath]
  , libraryDirectories :: [FilePath]
  , showHelp :: Bool
  , showVersion :: Bool
  }

main :: IO ()
main = do

  arguments <- parseArguments
  preludes <- locateImport "prelude"
    (libraryDirectories arguments)

  prelude <- if enableImplicitPrelude arguments
    then case preludes of
      [] -> do
        hPutStrLn stderr "No module 'prelude' found."
        exitFailure

      [filename] -> do

        source <- readFile filename
        mPrelude <- compile Compile.Config
          { Compile.dumpResolved = dumpResolved arguments
          , Compile.dumpScoped = dumpScoped arguments
          , Compile.name = filename
          , Compile.prelude = mempty
          , Compile.source = source
          }

        Fragment{..} <- case mPrelude of
          Left compileErrors -> do
            printCompileErrors compileErrors
            exitFailure
          Right prelude -> return prelude

        unless (null fragmentTerms) $ do
          hPutStrLn stderr "Prelude includes executable code."
          exitFailure

        return mempty { fragmentDefs = fragmentDefs }

      _ -> do
        hPutStrLn stderr . unlines
          $ "Too many prelude candidates:"
          : preludes
        exitFailure

    else return mempty

  forM_ (entryPoints arguments) $ \ filename -> do
    program <- readFile filename
    mResult <- compile Compile.Config
      { Compile.dumpResolved = dumpResolved arguments
      , Compile.dumpScoped = dumpScoped arguments
      , Compile.name = filename
      , Compile.prelude = prelude
      , Compile.source = program
      }
    case mResult of
      Left compileErrors -> printCompileErrors compileErrors
      Right result -> case compileMode arguments of
        CompileMode -> mapM_ print $ yarn result
        InterpretMode -> interpret [] prelude result

printCompileErrors :: [CompileError] -> IO ()
printCompileErrors errors
  = hPutStr stderr $ unlines (map show errors)

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
    { compileMode = InterpretMode
    , dumpResolved = False
    , dumpScoped = False
    , enableImplicitPrelude = True
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
    [ flagBool' ["c", "compile"]
      "Compile Yarn assembly."
      $ \ flag acc@Arguments{..} -> acc
      { compileMode = if flag then CompileMode else InterpretMode }

    , flagBool' ["dump-resolved"]
      "Output result of name resolution."
      $ \ flag acc@Arguments{..} -> acc
      { dumpResolved = flag }

    , flagBool' ["dump-scoped"]
      "Output result of scope resolution."
      $ \ flag acc@Arguments{..} -> acc
      { dumpScoped = flag }

    , flagReq' ["L", "library"] "DIR"
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
