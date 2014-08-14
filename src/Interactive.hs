{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive
  ( runInteraction
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Prelude hiding (interact)
import System.Console.Haskeline
import Text.Parsec.Pos
import Text.PrettyPrint.Boxes hiding ((<>))

import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Kitten.Compile (compile)
import Kitten.Error
import Kitten.Interpret
import Kitten.IR
import Kitten.Location
import Kitten.Type.Tidy
import Kitten.Types
import Kitten.Util.List
import Kitten.Util.Monad

import qualified Kitten.Interpret as Interpret

data Env = Env
  { envLine :: !Int
  , envProgram :: !Program
  , envStack :: [InterpreterValue]
  , envConfig :: Config
  }

type State = StateT Env IO
type Input = InputT State

runInteraction :: Config -> IO ()
runInteraction config = do
  welcome
  flip evalStateT (emptyEnv config) $ runInputT settings $ do
    eval $ if configImplicitPrelude config then "import Prelude" else ""
    interact

  where
  welcome :: IO ()
  welcome = mapM_ T.putStrLn
    [ "Welcome to Kitten!"
    , "Type '" <> commandPrefix <> "help' for help \
      \or '" <> commandPrefix <> "quit' to quit."
    ]

emptyEnv :: Config -> Env
emptyEnv config = Env
  { envConfig = config
    { configImplicitPrelude = False
    , configOptimizations = defaultOptimizations
      { optUnusedDefElim = False  -- Bad for interactive mode.
      }
    }
  , envLine = 1
  , envProgram = emptyProgram
  , envStack = []
  }

settings :: Settings State
settings = setComplete completer $ defaultSettings
  { autoAddHistory = True
  , historyFile = Nothing
  }

completer :: CompletionFunc State
completer = completeWord Nothing "\t \"{}[]()\\" completePrefix

completePrefix :: String -> State [Completion]
completePrefix prefix
  | Just rest <- T.stripPrefix commandPrefix (T.pack prefix) = let
  commandNames = concatMap (S.toList . cmdSymbols) replCommands
  matching = filter (rest `T.isPrefixOf`) commandNames
  in return $ map
    (toCompletion (small matching) . T.unpack . (commandPrefix <>)) matching
  | otherwise = do
  symbols <- gets (H.keys . programSymbols . envProgram)
  let
    prefix' = T.pack prefix
    matching
      = filter (prefix' `T.isPrefixOf`) symbols
      <> filter (prefix' `T.isPrefixOf`) intrinsicNameList
    completions = map (toCompletion (small matching) . T.unpack) matching
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
    Just input -> evalOrContinue 0 (T.pack input) ""

splitCommandArgs :: Text -> Maybe (Text, Text)
splitCommandArgs line
  | Just unprefixed <- T.stripPrefix commandPrefix line
  = let (command, args) = T.break isSpace unprefixed
  in Just (command, T.strip args)
  | otherwise = Nothing

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
continue offset acc = do
  mLine <- getInput offset "..."
  case mLine of
    Nothing -> quit
    Just line -> evalOrContinue offset (T.pack line) acc

evalOrContinue :: Int -> Text -> Text -> Input ()
evalOrContinue offset line acc
  | Just (command, args) <- splitCommandArgs line
  , Just Command{..} <- lookup command replCommandsTable
  = cmdAction args
  | matched (T.unpack acc') = eval acc' >> interact'
  | otherwise = continue (succ offset) acc'
  where acc' = T.concat [acc, "\n", line]

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
      { envLine = envLine s + T.count "\n" input
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
  return envConfig { configFirstLine = envLine }

liftIO :: IO a -> Input a
liftIO = lift . lift

type LineArgs = Text
type Interaction = LineArgs -> Input ()

data Command = Command
  { cmdSymbols :: Set Text
  , cmdAction :: Interaction
  , cmdArgs :: [Text]
  , cmdDesc :: Text
  }

newArgCommand :: [Text] -> Interaction -> [Text] -> Text -> Command
newArgCommand symbols func helpArgs helpText = Command
  { cmdSymbols = S.fromList symbols
  , cmdAction = func
  , cmdArgs = helpArgs
  , cmdDesc = helpText
  }

commandPrefix :: Text
commandPrefix = ":"

newCommand :: [Text] -> Input () -> Text -> Command
newCommand symbols func = newArgCommand symbols (const func) []

replCommands :: [Command]
replCommands =
  [ newCommand ["c", "clear"] clear "Clear the stack"
  , newCommand ["h", "help"] help "Display this help message"
  , newCommand ["q", "quit"] quit "Quit interactive mode"
  , newArgCommand ["l", "load"] (load . T.unpack)
    ["filepath"] "Load a file into interactive mode"
  , newCommand ["reset"] reset "Clear the stack and all definitions"
  , newArgCommand ["t", "type"] reportType
      ["expression"] "Print the inferred type of <expression>"
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
  lift $ put . emptyEnv =<< gets envConfig
  interact'

load :: FilePath -> Input ()
load file = do
  r <- liftIO (E.try (T.readFile file) :: IO (Either IOException Text))
  case r of
    Left e -> do
      liftIO . putStrLn $ "Error loading file:\n  " ++ show e
      interact'
    Right contents -> do
      liftIO . putStrLn $ "File loaded: " ++ file
      eval contents >> interact'

help :: Input ()
help = do
  liftIO $ do
    putStrLn ""
    putStr . render . vcat left . intersperse vpad
      $ map (hcat top . intersperse hpad . map (vcat left) . transpose)
      [ [ colspan 2 (text "Kitten Forms")
        , [ text "<expression>"
          , text "Evaluate <expression> and print the result"
          ]
        , [ text "def <name> (<signature>) <body>"
          , text "Introduce a definition"
          ]
        ]
      , colspan 3 (text "Interactive Commands")
        : for replCommands
        (\Command{..} ->
          [ text . T.unpack $ T.concat
            [ "["
            , T.intercalate ", " . map (commandPrefix <>) $ S.toList cmdSymbols
            , "]"
            ]
          , text . T.unpack . T.intercalate " "
            $ map (("<" <>) . (<> ">")) cmdArgs
          , text (T.unpack cmdDesc)
          ])
      , [ colspan 2 (text "Keybindings")
        , [ text "<TAB>"
          , "Autocomplete a definition name"
          ]
        ]
      ]
    putStrLn ""
  interact'
  where
  vpad = emptyBox vmargin 0
  vmargin = 1
  hpad = emptyBox 0 hmargin
  hmargin = 2
  colspan n = (: replicate (n - 1) (text ""))

reportType :: Interaction
reportType input = do
  mCompiled <- interactiveCompile $ \config -> config
    { configEnforceBottom = False
    , configSource = input
    }
  whenJust mCompiled $ \(_compiled, _ip, type_)
    -> liftIO . print . runTidy $ tidyType type_
  interact'
