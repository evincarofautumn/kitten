{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Repl
  ( runRepl
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Monoid
import Data.Char (isSpace)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import System.Console.Haskeline

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import qualified Control.Exception as E

import Kitten.Compile (compile)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Interpret.Monad (InterpreterValue)
import Kitten.Name (NameGen)
import Kitten.Type
import Kitten.TypeDef
import Kitten.Typed (Typed, TypedDef)
import Kitten.Util.Monad

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Compile as Compile
import qualified Kitten.Infer.Config as Infer
import qualified Kitten.Interpret.Monad as Interpret

data Repl = Repl
  { replDefs :: !(Vector TypedDef)
  , replLine :: !Int
  , replNameGen :: !NameGen
  , replStack :: [InterpreterValue]
  , replTypeDefs :: !(Vector TypeDef)
  }

type ReplReader = ReaderT (Vector TypedDef) IO
type ReplState = StateT Repl ReplReader
type ReplInput = InputT ReplState

liftIO :: IO a -> ReplInput a
liftIO = lift . lift . lift

runRepl :: Fragment Typed -> NameGen -> IO ()
runRepl prelude nameGen = do
  welcome
  flip runReaderT preludeDefs
    . flip evalStateT emptyRepl
    $ runInputT settings repl

  where
  emptyRepl :: Repl
  emptyRepl = Repl
    { replDefs = preludeDefs
    , replLine = 1
    , replNameGen = nameGen
    , replStack = []
    , replTypeDefs = V.empty
    }

  welcome :: IO ()
  welcome = mapM_ putStrLn
    [ "Welcome to Kitten!"
    , "Type ':help' for help or ':quit' to quit."
    ]

  preludeDefs = fragmentDefs prelude

type LineArgs = Text
type ReplAction = LineArgs -> ReplInput ()

data ReplCommandDesc = ReplCommandDesc
  { descCommand :: Text
  , descHelp :: Text
  }

data ReplCommand = ReplCommand
  { cmdSymbols :: Set Text
  , cmdAction :: ReplAction
  , cmdDesc :: ReplCommandDesc
  }

newArgCommand :: [Text] -> ReplAction -> Text -> Text -> ReplCommand
newArgCommand symbols func helpArgs helpText =
  ReplCommand
  { cmdSymbols = S.fromList fmtSymbols
  , cmdAction = func
  , cmdDesc = ReplCommandDesc
    { descCommand = T.unwords [symbolText, helpArgs]
    , descHelp = helpText
    }
  }
  where
  fmtSymbols = map (T.cons ':') symbols
  symbolText = T.concat ["[", T.intercalate ", " fmtSymbols, "]"]

newCommand :: [Text] -> ReplInput () -> Text -> ReplCommand
newCommand symbols func helpText =
  newArgCommand symbols (const func) "" helpText

replCommands :: [ReplCommand]
replCommands =
  [ newCommand ["c", "clear"] clear "Clear the Stack"
  , newCommand ["h", "help"] help "Display this help message"
  , newCommand ["q", "quit"] quit "Quit the Kitten REPL"
  , newArgCommand ["l", "load"] (load . T.unpack)
      "<filepath>" "Load a file into the Kitten REPL"
  , newCommand ["reset"] reset "Clear the stack and all definitions"
  , newArgCommand ["t", "type"] typeOf
      "<expression>" "Print the inferred type of <expression>"
  ]

replCommandsTable :: [(Text, ReplCommand)]
replCommandsTable = toTable $ zipSymbols replCommands
  where
  zipSymbols xs = zip (map (S.toList . cmdSymbols) xs) xs
  toTable = concatMap (\(ks, v) -> map (\k -> (k, v)) ks)

repl :: ReplInput ()
repl = do
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just input -> evaluate input
  where
  evaluate line = case lookup cmd replCommandsTable of
    Just (ReplCommand {cmdAction=execute}) -> execute args
    Nothing
      | not (matched line) -> continue (T.pack line)
      | null line -> repl'
      | otherwise -> eval (T.pack line)
    where
    (cmd, args) =
      let (c, a) = T.break isSpace $ T.pack line
      in (c, T.strip a)

repl' :: ReplInput ()
repl' = showStack >> repl

askPreludeDefs :: ReplInput (Vector TypedDef)
askPreludeDefs = lift (lift ask)

matched :: String -> Bool
matched = go False (0::Int)
  where
  go q n ('\\':x:xs)
    | x `elem` "'\"" = go q n xs
    | otherwise = go q n xs
  go q n ('"':xs) = go (not q) n xs
  go True n (_:xs) = go True n xs
  go True _ [] = True
  go False n (x:xs)
    | isOpen x = go False (succ n) xs
    | isClose x = n <= 0 || go False (pred n) xs
    | otherwise = go False n xs
  go False n [] = n == 0
  isOpen = (`elem` "([{")
  isClose = (`elem` "}])")

compileConfig :: ReplInput Compile.Config
compileConfig = do
  Repl{..} <- lift get
  return Compile.Config
    { Compile.dumpResolved = False
    , Compile.dumpScoped = False
    , Compile.firstLine = replLine
    , Compile.inferConfig = Infer.Config { enforceBottom = True }
    , Compile.libraryDirectories = []  -- TODO
    , Compile.name = replName
    , Compile.prelude = mempty
      { fragmentDefs = replDefs
      , fragmentTypeDefs = replTypeDefs
      }
    , Compile.source = ""
    , Compile.stackTypes = V.empty
    }

replCompile
  :: (Compile.Config -> Compile.Config)
  -> ReplInput (Maybe (Fragment Typed, Type Scalar))
replCompile update = do
  nameGen <- lift $ gets replNameGen
  mCompiled <- liftIO . flip compile nameGen . update =<< compileConfig
  case mCompiled of
    Left errors -> liftIO (printCompileErrors errors) >> return Nothing
    Right (nameGen', typed, type_) -> do
      lift . modify $ \env -> env { replNameGen = nameGen' }
      return $ Just (typed, type_)

typeOf :: ReplAction
typeOf line = do
  mCompiled <- replCompile $ \config -> config
    { Compile.source = line
    , Compile.inferConfig = Infer.Config { enforceBottom = False }
    }
  whenJust mCompiled $ \(_compiled, type_) -> liftIO $ print type_
  repl'

eval :: ReplAction
eval line = do
  mCompiled <- do
    nameGen <- lift $ gets replNameGen
    stackValues <- lift $ gets replStack
    let
      (stackTypes, nameGen') = flip runState nameGen
        $ mapM (state . Interpret.typeOfValue) stackValues
    lift . modify $ \env -> env { replNameGen = nameGen' }
    replCompile $ \config -> config
      { Compile.source = line
      , Compile.stackTypes = V.fromList (reverse stackTypes)
      }

  whenJust mCompiled $ \(compiled, _type) -> do
    Repl{..} <- lift get
    stack' <- liftIO $ interpret replStack mempty
      { fragmentDefs = replDefs
      , fragmentTypeDefs = replTypeDefs
      } compiled
    lift . modify $ \s -> s
      { replDefs = replDefs <> fragmentDefs compiled
      , replLine = replLine + T.count "\n" line + 1
      , replStack = stack'
      , replTypeDefs = replTypeDefs <> fragmentTypeDefs compiled
      }
  repl'

continue :: ReplAction
continue prefix = do
  mLine <- getInputLine "... "
  case mLine of
    Nothing -> quit
    Just line -> let whole = T.unlines [prefix, T.pack line]
      in (if matched (T.unpack whole)
        then eval else continue) whole

showStack :: ReplInput ()
showStack = do
  stack <- lift $ gets replStack
  unless (null stack) . liftIO $ do
    putStrLn "\n----"
    mapM_ putStrLn . reverse $ map show stack

replName :: String
replName = "REPL"

help :: ReplInput ()
help = do
  liftIO $ printColumns $ concat
    [ [ Just ("<expression>", "Evaluate <expression> and print the result")
      , Just ("def <name> (<signature>) <body>", "Introduce a definition")
      , Nothing
      ]
    , replCommandsHelp
    , [ Nothing
      , Just ("<TAB>", "Autocomplete a definition name")
      ]
    ]
  repl'
  where
  printColumns :: [Maybe (String, String)] -> IO ()
  printColumns columns = mapM_ go columns
    where
    margin = 2
    width = maximum . map (length . fst) $ catMaybes columns
    go column = case column of
      Nothing -> putStrLn ""
      Just (a, b) -> do
        putStr a
        putStr (replicate (width + margin - length a) ' ')
        putStrLn b
  replCommandsHelp =
    map (\(ReplCommand {cmdDesc=(ReplCommandDesc fmt info)}) ->
      Just (T.unpack fmt, T.unpack info)) replCommands

quit :: ReplInput ()
quit = return ()

clear :: ReplInput ()
clear = do
  lift . modify $ \s -> s { replStack = [] }
  repl'

reset :: ReplInput ()
reset = do
  preludeDefs <- askPreludeDefs
  nameGen <- lift $ gets replNameGen
  lift $ put Repl
    { replDefs = preludeDefs
    , replLine = 1
    , replNameGen = nameGen
    , replStack = []
    , replTypeDefs = V.empty
    }
  repl'

load :: FilePath -> ReplInput ()
load file = do
  r <- liftIO $ (E.try (TIO.readFile file) :: IO (Either IOException Text))
  case r of
    Left e -> liftIO (putStrLn $ "Error loading file:\n  " ++ show e) >> repl'
    Right contents -> liftIO (putStrLn $ "File loaded: " ++ file) >> eval contents

completer :: CompletionFunc ReplState
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix :: String -> ReplState [Completion]
completePrefix prefix = do
  defs <- gets replDefs
  let
    prefix' = T.pack prefix
    matching
      = V.filter (prefix' `T.isPrefixOf`) (defName <$> defs)
      <> V.filter (prefix' `T.isPrefixOf`) Builtin.names
    finished = V.length matching <= 1
    completions = map (toCompletion finished . T.unpack) (V.toList matching)
  return completions

toCompletion :: Bool -> String -> Completion
toCompletion finished name = Completion
  { replacement = name
  , display = name
  , isFinished = finished
  }

settings :: Settings ReplState
settings = Settings
  { complete = completer
  , historyFile = Nothing
  , autoAddHistory = True
  }
