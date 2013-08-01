{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Monoid
import System.Console.CmdArgs.Explicit
import System.Exit
import System.IO

import Kitten.Compile (compile, locateImport)
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Resolved (Resolved, Value)
import Kitten.Yarn (yarn)
import Repl

import qualified Kitten.Compile as Compile

data CompileMode
  = CompileMode
  | InterpretMode

data Arguments = Arguments
  { argsCompileMode :: CompileMode
  , argsDumpResolved :: Bool
  , argsDumpScoped :: Bool
  , argsEnableImplicitPrelude :: Bool
  , argsEntryPoints :: [FilePath]
  , argsLibraryDirectories :: [FilePath]
  , argsShowHelp :: Bool
  , argsShowVersion :: Bool
  }

main :: IO ()
main = do

  arguments <- parseArguments
  preludes <- locateImport
    (argsLibraryDirectories arguments)
    "Prelude"

  prelude <- if not (argsEnableImplicitPrelude arguments)
    then return mempty
    else case preludes of

    [] -> do
      hPutStrLn stderr "No module 'Prelude' found."
      exitFailure

    [filename] -> do
      source <- readFile filename
      mPrelude <- compile Compile.Config
        { Compile.dumpResolved = argsDumpResolved arguments
        , Compile.dumpScoped = argsDumpScoped arguments
        , Compile.libraryDirectories
          = argsLibraryDirectories arguments
        , Compile.name = filename
        , Compile.prelude = mempty
        , Compile.source = source
        , Compile.stack = []
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
        $ "Too many Prelude candidates:"
        : preludes
      exitFailure

  case argsEntryPoints arguments of
    [] -> runRepl prelude
    entryPoints -> interpretAll entryPoints
      (argsCompileMode arguments) prelude
      $ \ filename program -> Compile.Config
      { Compile.dumpResolved = argsDumpResolved arguments
      , Compile.dumpScoped = argsDumpScoped arguments
      , Compile.libraryDirectories = argsLibraryDirectories arguments
      , Compile.name = filename
      , Compile.prelude = prelude
      , Compile.source = program
      , Compile.stack = []
      }

interpretAll
  :: [FilePath]
  -> CompileMode
  -> Fragment Value Resolved
  -> (FilePath -> String -> Compile.Config)
  -> IO ()
interpretAll entryPoints compileMode prelude config
  = mapM_ interpretOne entryPoints
  where
  interpretOne :: FilePath -> IO ()
  interpretOne filename = do
    program <- readFile filename
    mResult <- compile (config filename program)
    case mResult of
      Left compileErrors -> printCompileErrors compileErrors
      Right result -> case compileMode of
        CompileMode -> mapM_ print $ yarn result
        InterpretMode -> void $ interpret [] prelude result

parseArguments :: IO Arguments
parseArguments = do
  arguments <- processArgs argumentsMode

  when (argsShowVersion arguments) $ do
    putStrLn "Kitten version 1.0"
    exitSuccess

  when (argsShowHelp arguments) $ do
    print $ helpText [] HelpFormatDefault argumentsMode
    exitSuccess

  return arguments

argumentsMode :: Mode Arguments
argumentsMode = mode "kitten" defaultArguments
  "Interprets Kitten code." bareArgument options
  where

  defaultArguments :: Arguments
  defaultArguments = Arguments
    { argsCompileMode = InterpretMode
    , argsDumpResolved = False
    , argsDumpScoped = False
    , argsEnableImplicitPrelude = True
    , argsEntryPoints = []
    , argsLibraryDirectories = []
    , argsShowHelp = False
    , argsShowVersion = False
    }

  bareArgument :: Arg Arguments
  bareArgument = flagArg entryPointArgument "entry-point"

  entryPointArgument
    :: FilePath -> Arguments -> Either e Arguments
  entryPointArgument path acc = Right
    $ acc { argsEntryPoints = path : argsEntryPoints acc }

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
      { argsCompileMode = if flag then CompileMode else InterpretMode }

    , flagBool' ["dump-resolved"]
      "Output result of name resolution."
      $ \ flag acc@Arguments{..} -> acc
      { argsDumpResolved = flag }

    , flagBool' ["dump-scoped"]
      "Output result of scope resolution."
      $ \ flag acc@Arguments{..} -> acc
      { argsDumpScoped = flag }

    , flagReq' ["L", "library"] "DIR"
      "Add library search directory."
      $ \ path acc@Arguments{..} -> Right $ acc
      { argsLibraryDirectories = path : argsLibraryDirectories }

    , flagBool' ["no-implicit-prelude"]
      "Disable implicit inclusion of prelude."
      $ \ flag acc@Arguments{..} -> acc
      { argsEnableImplicitPrelude = not flag }

    , flagHelpSimple $ \ acc -> acc { argsShowHelp = True }
    , flagVersion $ \ acc -> acc { argsShowVersion = True }
    ]
