{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive
  ( runInteraction
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Char (isSpace)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Prelude hiding (interact)
import System.Console.Haskeline
import Text.Parsec.Pos

import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import Kitten.Compile (compile)
import Kitten.Error
import Kitten.Interpret
import Kitten.IR
import Kitten.Location
import Kitten.Types
import Kitten.Util.Monad

import qualified Kitten.Interpret as Interpret

data Env = Env
  { envLine :: !Int
  , envProgram :: !Program
  , envStack :: [InterpreterValue]
  }

type State = StateT Env IO
type Input = InputT State

runInteraction :: Bool -> IO ()
runInteraction implicitPrelude = do
  welcome
  flip evalStateT emptyEnv $ runInputT settings $ do
    when implicitPrelude $ eval "import Prelude"
    interact

  where
  welcome :: IO ()
  welcome = mapM_ putStrLn
    [ "Welcome to Kitten!"
    , "Type ':help' for help or ':quit' to quit."
    ]

emptyEnv :: Env
emptyEnv = Env
  { envLine = 0
  , envProgram = emptyProgram
  , envStack = []
  }

settings :: Settings State
settings = setComplete completer $ defaultSettings
  { autoAddHistory = True
  , historyFile = Nothing
  }

completer :: CompletionFunc State
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix :: String -> State [Completion]
completePrefix prefix = do
  symbols <- gets (H.keys . programSymbols . envProgram)
  let
    prefix' = T.pack prefix
    matching
      = filter (prefix' `T.isPrefixOf`) symbols
      <> filter (prefix' `T.isPrefixOf`) intrinsicNameList
    finished = case matching of [] -> True; [_] -> True; _ -> False
    completions = map (toCompletion finished . T.unpack) matching
  return completions

intrinsicNameList :: [Text]
intrinsicNameList = V.toList intrinsicNames

toCompletion :: Bool -> String -> Completion
toCompletion finished name = Completion
  { replacement = name
  , display = name
  , isFinished = finished
  }

interact :: Input ()
interact = do
  mLine <- getInput 0 ">>>"
  case mLine of
    Nothing -> quit
    Just input -> evaluate input
  where
  evaluate line = case lookup command replCommandsTable of
    Just Command{..} -> cmdAction args
    Nothing
      | not (matched line) -> continue 1 line'
      | otherwise -> eval line' >> interact'
    where
    line' = T.pack line
    (command, args) = splitCommandArgs line'

splitCommandArgs :: Text -> (Text, Text)
splitCommandArgs line = let
  (command, args) = T.break isSpace line
  in (command, T.strip args)

data InString = Inside | Outside

matched :: String -> Bool
matched = go Outside (0::Int)
  where
  go q n ('\\':x:xs)
    | x `elem` "'\"" = go q n xs
    | otherwise = go q n xs
  go q n ('"':xs) = go (case q of Inside -> Outside; Outside -> Inside) n xs
  go Inside n (_:xs) = go Inside n xs
  go Inside _ [] = True
  go Outside n (x:xs)
    | isOpen x = go Outside (succ n) xs
    | isClose x = n <= 0 || go Outside (pred n) xs
    | otherwise = go Outside n xs
  go Outside n [] = n == 0
  isOpen = (`elem` "([{")
  isClose = (`elem` "}])")

continue :: Int -> Text -> Input ()
continue offset prefix = do
  mLine <- getInput offset "..."
  case mLine of
    Nothing -> quit
    Just line -> let
      line' = T.pack line
      whole = T.concat [prefix, "\n", line']
      (command, args) = splitCommandArgs line'
      in case lookup command replCommandsTable of
        Just Command{..} -> cmdAction args
        Nothing -> if matched (T.unpack whole)
          then eval whole >> interact'
          else continue (succ offset) whole

getInput :: Int -> String -> Input (Maybe String)
getInput offset prompt = do
  lineNumber <- lift $ gets (show . (+ offset) . envLine)
  getInputLine $ concat
    [replicate (3 - length lineNumber) ' ', lineNumber, " ", prompt, " "]

eval :: Interaction
eval input = do
  mCompiled <- do
    idGen <- lift $ gets (programScalarIdGen . envProgram)
    stackValues <- lift $ gets envStack
    lineNumber <- lift $ gets envLine
    let
      loc = Location
        { locationStart = newPos "" lineNumber 0
        , locationIndent = -1
        }
      (stackTypes, idGen') = flip runState idGen
        $ mapM (state . Interpret.typeOf loc) stackValues
    lift . modify $ \env -> env
      { envProgram = (envProgram env)
        { programScalarIdGen = idGen' }
      }
    interactiveCompile $ \config -> config
      { configSource = input
      , configStackTypes = V.fromList (reverse stackTypes)
      }
  whenJust mCompiled $ \ (compiled, ip, _type) -> do
    stackState <- lift $ gets envStack
    stackState' <- liftIO $ interpret (Just ip) stackState compiled
    lift . modify $ \s -> s
      { envLine = envLine s + T.count "\n" input + 1
      , envStack = stackState'
      }

interact' :: Input ()
interact' = showStack >> interact

quit :: Input ()
quit = noop

showStack :: Input ()
showStack = do
  data_ <- lift $ gets envStack
  unless (null data_) . liftIO $ do
    putStrLn "\n----"
    mapM_ putStrLn . reverse $ map show data_

interactiveCompile
  :: (Config -> Config)
  -> Input (Maybe (Program, Int, Type Scalar))
interactiveCompile update = do
  program <- lift $ gets envProgram
  mCompiled <- do
    config <- compileConfig
    liftIO $ compile (update config) program
  case mCompiled of
    Left errors -> liftIO (printCompileErrors errors) >> return Nothing
    Right result@(program', _, _) -> do
      lift . modify $ \env -> env { envProgram = program' }
      return (Just result)

compileConfig :: Input Config
compileConfig = do
  Env{..} <- lift get
  return Config
    { configDumpResolved = False
    , configDumpScoped = False
    , configEnforceBottom = True
    , configFirstLine = envLine
    , configImplicitPrelude = False
    , configLibraryDirectories = []  -- TODO
    , configOptimizations = defaultOptimizations
      { optUnusedDefElim = False  -- Bad for interactive mode.
      }
    , configName = ""
    , configPredefined = mempty  -- TODO
    , configSource = ""
    , configStackTypes = V.empty
    }

liftIO :: IO a -> Input a
liftIO = lift . lift

type LineArgs = Text
type Interaction = LineArgs -> Input ()

data Description = Description
  { descCommand :: Text
  , descHelp :: Text
  }

data Command = Command
  { cmdSymbols :: Set Text
  , cmdAction :: Interaction
  , cmdDesc :: Description
  }

newArgCommand :: [Text] -> Interaction -> Text -> Text -> Command
newArgCommand symbols func helpArgs helpText = Command
  { cmdSymbols = S.fromList fmtSymbols
  , cmdAction = func
  , cmdDesc = Description
    { descCommand = T.unwords [symbolText, helpArgs]
    , descHelp = helpText
    }
  }
  where
  fmtSymbols = map (T.cons ':') symbols
  symbolText = T.concat ["[", T.intercalate ", " fmtSymbols, "]"]

newCommand :: [Text] -> Input () -> Text -> Command
newCommand symbols func = newArgCommand symbols (const func) ""

replCommands :: [Command]
replCommands =
  [ newCommand ["c", "clear"] clear "Clear the stack"
  , newCommand ["h", "help"] help "Display this help message"
  , newCommand ["q", "quit"] quit "Quit interactive mode"
  , newArgCommand ["l", "load"] (load . T.unpack)
      "<filepath>" "Load a file into interactive mode"
  , newCommand ["reset"] reset "Clear the stack and all definitions"
  , newArgCommand ["t", "type"] reportType
      "<expression>" "Print the inferred type of <expression>"
  ]

replCommandsTable :: [(Text, Command)]
replCommandsTable = toTable $ zipSymbols replCommands
  where
  zipSymbols xs = zip (map (S.toList . cmdSymbols) xs) xs
  toTable = concatMap (\(ks, v) -> map (\k -> (k, v)) ks)

clear :: Input ()
clear = do
  lift . modify $ \s -> s { envStack = [] }
  interact'

reset :: Input ()
reset = do
  lift $ put emptyEnv
  interact'

load :: FilePath -> Input ()
load file = do
  r <- liftIO (E.try (TIO.readFile file) :: IO (Either IOException Text))
  case r of
    Left e -> do
      liftIO . putStrLn $ "Error loading file:\n  " ++ show e
      interact'
    Right contents -> do
      liftIO . putStrLn $ "File loaded: " ++ file
      eval contents >> interact'

help :: Input ()
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
  interact'

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
    map (\(Command {cmdDesc=(Description fmt info)}) ->
      Just (T.unpack fmt, T.unpack info)) replCommands

reportType :: Interaction
reportType input = do
  mCompiled <- interactiveCompile $ \config -> config
    { configEnforceBottom = False
    , configSource = input
    }
  whenJust mCompiled $ \(_compiled, _ip, type_) -> liftIO $ print type_
  interact'
