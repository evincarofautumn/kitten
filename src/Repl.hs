{-# LANGUAGE RecordWildCards #-}

module Repl
  ( runRepl
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.Console.Haskeline

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Compile (compile)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Resolved
import Kitten.TypeDef

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Compile as Compile

data Repl = Repl
  { replStack :: [Value]
  , replDefs :: !(Vector (Def Value))
  , replTypeDefs :: !(Vector TypeDef)
  }

type ReplReader = ReaderT (Vector (Def Value)) IO
type ReplState = StateT Repl ReplReader
type ReplInput = InputT ReplState

liftIO :: IO a -> ReplInput a
liftIO = lift . lift . lift

runRepl :: Fragment Value Resolved -> IO ()
runRepl prelude = do
  welcome
  flip runReaderT preludeDefs
    . flip evalStateT emptyRepl
    $ runInputT settings repl

  where
  emptyRepl :: Repl
  emptyRepl = Repl
    { replStack = []
    , replDefs = preludeDefs
    , replTypeDefs = V.empty
    }

  welcome :: IO ()
  welcome = mapM_ putStrLn
    [ "Welcome to Kitten!"
    , "Type ':help' for help or ':quit' to quit."
    ]

  preludeDefs = fragmentDefs prelude

repl :: ReplInput ()
repl = do
  showStack
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just line
      | not (matched line) -> continue (T.pack line)
      | null line -> repl
      | line `elem` [":c", ":clear"] -> clear
      | line `elem` [":h", ":help"] -> help
      | line `elem` [":q", ":quit"] -> quit
      | line `elem` [":reset"] -> reset
      | otherwise -> eval (T.pack line)

askPreludeDefs :: ReplInput (Vector (Def Value))
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
    | isClose x = n < 0 || go False (pred n) xs
    | otherwise = go False n xs
  go False n [] = n == 0
  isOpen = (`elem` "([{")
  isClose = (`elem` "}])")

eval :: Text -> ReplInput ()
eval line = do
  Repl{..} <- lift get
  mCompiled <- liftIO $ compile Compile.Config
    { Compile.dumpResolved = False
    , Compile.dumpScoped = False
    , Compile.libraryDirectories = []  -- TODO
    , Compile.name = replName
    , Compile.prelude = mempty
      { fragmentDefs = replDefs
      , fragmentTypeDefs = replTypeDefs
      }
    , Compile.source = line
    , Compile.stack = replStack
    }
  case mCompiled of
    Left compileErrors -> liftIO
      $ printCompileErrors compileErrors
    Right compileResult -> do
      stack' <- liftIO $ interpret replStack mempty
        { fragmentDefs = replDefs
        , fragmentTypeDefs = replTypeDefs
        } compileResult
      lift . modify $ \ s -> s
        { replStack = stack'
        , replDefs = replDefs <> fragmentDefs compileResult
        , replTypeDefs = replTypeDefs <> fragmentTypeDefs compileResult
        }
  repl

continue :: Text -> ReplInput ()
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
  liftIO $ do
    putStrLn "----"
    mapM_ putStrLn . reverse $ map show stack

replName :: String
replName = "REPL"

help :: ReplInput ()
help = do
  liftIO $ printColumns
    [ Just ("<expression>", "Evaluate <expression> and print the result")
    , Just ("def <name> (<signature>) <body>", "Introduce a definition")
    , Nothing
    , Just (":c, :clear", "Clear the stack")
    , Just (":h, :help", "Display this help message")
    , Just (":q, :quit", "Quit the Kitten REPL")
    , Just (":reset", "Clear the stack and all definitions")
    , Nothing
    , Just ("<TAB>", "Autocomplete a definition name")
    ]
  repl
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

quit :: ReplInput ()
quit = return ()

clear :: ReplInput ()
clear = do
  lift . modify $ \ s -> s { replStack = [] }
  repl

reset :: ReplInput ()
reset = do
  preludeDefs <- askPreludeDefs
  lift $ put Repl
    { replStack = []
    , replDefs = preludeDefs
    , replTypeDefs = V.empty
    }
  repl

completer :: CompletionFunc ReplState
completer = completeWord Nothing "\t \"{}[]()\\:" completePrefix

completePrefix
  :: String
  -> StateT Repl (ReaderT (Vector (Def Value)) IO) [Completion]
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
