{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import System.IO

import Term

import qualified Token

import qualified Text.Parsec as P

main :: IO ()
main = fix $ \ loop -> do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      putStrLn $ case compile "STDIN" line of
        Left compileError -> show compileError
        Right compileResult -> show compileResult
      loop

data CompileError
  = CompileError String

instance Show CompileError where
  show (CompileError message) = message

compile :: String -> String -> Either CompileError Program
compile name
  = failIfError . Token.tokenize name
  >=> failIfError . parse name
  >=> resolveProgram
  where
  failIfError = mapLeft (CompileError . show)
  resolveProgram (Program defs term) = Program defs <$> resolve defs term

class Resolvable a where
  resolve :: [Def] -> a -> Either CompileError a

instance (Resolvable a) => Resolvable [a] where
  resolve = mapM . resolve

instance Resolvable Def where
  resolve defs (Def name body)
    = Def name <$> resolve defs body

instance Resolvable Term where
  resolve defs (Word name Nothing) = case defIndex defs name of
    Just index -> Right $ Word name (Just index)
    Nothing -> Left . CompileError $ concat
      ["Unable to resolve word '", name, "'"]
  resolve _ term@(Word _ _) = Right term
  resolve defs (Fun terms) = Fun <$> resolve defs terms
  resolve defs (Vec terms) = Vec <$> resolve defs terms
  resolve defs (Compose down top)
    = Compose <$> resolve defs down <*> resolve defs top
  resolve _ term = Right term

defIndex :: [Def] -> String -> Maybe Int
defIndex defs expected = findIndex ((== expected) . defName) defs
  where defName (Def name _) = name

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
