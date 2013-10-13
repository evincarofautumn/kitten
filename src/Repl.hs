{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
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
import Kitten.Name (NameGen)
import Kitten.Resolved
import Kitten.Type
import Kitten.TypeDef
import Kitten.Util.Monad

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Compile as Compile
import qualified Kitten.Infer.Config as Infer

data Repl = Repl
  { replDefs :: !(Vector (Def Value))
  , replLine :: !Int
  , replNameGen :: !NameGen
  , replStack :: [Value]
  , replTypeDefs :: !(Vector TypeDef)
  }

type ReplReader = ReaderT (Vector (Def Value)) IO
type ReplState = StateT Repl ReplReader
type ReplInput = InputT ReplState

liftIO :: IO a -> ReplInput a
liftIO = lift . lift . lift

runRepl :: Fragment Resolved -> NameGen -> IO ()
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

repl :: ReplInput ()
repl = do
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> quit
    Just line
      | not (matched line) -> continue (T.pack line)
      | null line -> repl'
      | Just expr <- T.stripPrefix ":type" (T.pack line) -> typeOf expr
      | Just expr <- T.stripPrefix ":t" (T.pack line) -> typeOf expr
      | line `elem` [":c", ":clear"] -> clear
      | line `elem` [":h", ":help"] -> help
      | line `elem` [":q", ":quit"] -> quit
      | line `elem` [":reset"] -> reset
      | otherwise -> eval (T.pack line)

repl' :: ReplInput ()
repl' = showStack >> repl

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
    , Compile.stack = []
    }

replCompile
  :: (Compile.Config -> Compile.Config)
  -> ReplInput (Maybe (Fragment Resolved, Type Scalar))
replCompile update = do
  nameGen <- lift $ gets replNameGen
  mCompiled <- liftIO . flip compile nameGen . update =<< compileConfig
  case mCompiled of
    Left errors -> liftIO (printCompileErrors errors) >> return Nothing
    Right (nameGen', compiled, type_) -> do
      lift . modify $ \env -> env { replNameGen = nameGen' }
      return $ Just (compiled, type_)

typeOf :: Text -> ReplInput ()
typeOf line = do
  mCompiled <- replCompile $ \config -> config
    { Compile.source = line
    , Compile.inferConfig = Infer.Config { enforceBottom = False }
    }
  whenJust mCompiled $ \(_compiled, type_) -> liftIO $ print type_
  repl'

eval :: Text -> ReplInput ()
eval line = do
  Repl{..} <- lift get
  mCompiled <- replCompile $ \config -> config
    { Compile.source = line
    , Compile.stack = replStack
    }
  whenJust mCompiled $ \(compiled, _type) -> do
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
  unless (null stack) . liftIO $ do
    putStrLn "\n----"
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
    , Just (":t, :type <expression>", "Print the inferred type of <expression>")
    , Nothing
    , Just ("<TAB>", "Autocomplete a definition name")
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
