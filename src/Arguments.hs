{-# LANGUAGE RecordWildCards #-}

module Arguments
  ( Arguments(..)
  , CompileMode(..)
  , OutputFormat(..)
  , parseArguments
  ) where

import Control.Monad
import System.Console.CmdArgs.Explicit
import System.Exit

data Arguments = Arguments
  { argsCompileMode :: CompileMode
  , argsDumpResolved :: Bool
  , argsDumpScoped :: Bool
  , argsDumpTyped :: Bool
  , argsEnableImplicitPrelude :: Bool
  , argsEntryPoints :: [FilePath]
  , argsLibraryDirectories :: [FilePath]
  , argsOutputPath :: Maybe FilePath
  , argsShowHelp :: Bool
  , argsShowVersion :: Bool
  }

data OutputFormat
  = OutputC
  | OutputIr

data CompileMode
  = CheckMode
  | CompileMode !OutputFormat
  | InterpretMode

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
    , argsDumpTyped = False
    , argsEnableImplicitPrelude = True
    , argsEntryPoints = []
    , argsLibraryDirectories = []
    , argsOutputPath = Nothing
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
    [ flagReq' ["c", "compile"] "c|ir"
      "Compile to the given output format."
      $ \format acc@Arguments{..} -> case format of
        "c" -> Right acc { argsCompileMode = CompileMode OutputC }
        "ir" -> Right acc { argsCompileMode = CompileMode OutputIr }
        _ -> Left $ "Unknown output format '" ++ format ++ "'."

    , flagBool' ["check"]
      "Check syntax and types without compiling or running."
      $ \flag acc@Arguments{..} -> acc
      { argsCompileMode = if flag then CheckMode else argsCompileMode }

    , flagBool' ["dump-resolved"]
      "Output result of name resolution."
      $ \flag acc@Arguments{..} -> acc
      { argsDumpResolved = flag }

    , flagBool' ["dump-scoped"]
      "Output result of scope resolution."
      $ \flag acc@Arguments{..} -> acc
      { argsDumpScoped = flag }

    , flagBool' ["dump-typed"]
      "Output result of type inference."
      $ \flag acc@Arguments{..} -> acc
      { argsDumpTyped = flag }

    , flagReq' ["L", "library"] "DIR"
      "Add library search directory."
      $ \path acc@Arguments{..} -> Right $ acc
      { argsLibraryDirectories = path : argsLibraryDirectories }

    , flagBool' ["no-implicit-prelude"]
      "Disable implicit inclusion of prelude."
      $ \flag acc@Arguments{..} -> acc
      { argsEnableImplicitPrelude = not flag }

    , flagReq' ["o", "output"] "PATH"
      "File path for compile output."
      $ \path acc@Arguments{..} -> case argsOutputPath of
        Just{} -> Left "Only one output path is allowed."
        Nothing -> Right $ acc { argsOutputPath = Just path }

    , flagHelpSimple $ \acc -> acc { argsShowHelp = True }
    , flagVersion $ \acc -> acc { argsShowVersion = True }
    ]
