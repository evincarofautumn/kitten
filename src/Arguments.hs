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
  { compileMode :: CompileMode
  , inputPaths :: [FilePath]
  , outputPath :: Maybe FilePath
  , showHelp :: Bool
  , showVersion :: Bool
  }

data CompileMode
  = CheckMode
  | CompileMode !OutputFormat
  | InterpretMode

data OutputFormat = OutputIr

parseArguments :: IO Arguments
parseArguments = do
  arguments <- processArgs argumentsMode
  when (showVersion arguments) $ do
    putStrLn "Kitten version 0.1"
    exitSuccess
  when (showHelp arguments) $ do
    print $ helpText [] HelpFormatDefault argumentsMode
    exitSuccess
  return arguments

argumentsMode :: Mode Arguments
argumentsMode = mode "kitten" defaultArguments
  "Compiles and interprets Kitten code." bareArgument options

defaultArguments :: Arguments
defaultArguments = Arguments
  { compileMode = InterpretMode
  , inputPaths = []
  , outputPath = Nothing
  , showHelp = False
  , showVersion = False
  }

bareArgument :: Arg Arguments
bareArgument = flagArg inputPathArgument "input-paths"

inputPathArgument
  :: FilePath -> Arguments -> Either e Arguments
inputPathArgument path acc = Right
  $ acc { inputPaths = path : inputPaths acc }

options :: [Flag Arguments]
options =
  [ flagReq' ["c", "compile"] "ir"
    "Compile to the given output format."
    $ \ format acc -> case format of
      "ir" -> Right acc { compileMode = CompileMode OutputIr }
      _ -> Left $ "Unknown output format '" ++ format ++ "'."

  , flagBool' ["check"]
    "Check syntax and types without compiling or running."
    $ \ flag acc -> acc
    { compileMode = if flag then CheckMode else compileMode acc }

  , flagReq' ["o", "output"] "PATH"
    "File path for compile output."
    $ \ path acc -> case outputPath acc of
      Just{} -> Left "Only one output path is allowed."
      Nothing -> Right $ acc { outputPath = Just path }

  , flagHelpSimple $ \ acc -> acc { showHelp = True }
  , flagVersion $ \ acc -> acc { showVersion = True }
  ]

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
