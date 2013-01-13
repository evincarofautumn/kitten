module Main where

import Control.Monad
import Data.Function
import System.IO

import Error
import Interpret
import Program
import Resolve (Resolved)

import qualified Resolve as Resolved
import qualified Term
import qualified Type as Typed
import qualified Token

main :: IO ()
main = fix $ \ loop -> do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      case compile "STDIN" line of
        Left compileError -> print compileError
        Right compileResult -> interpret compileResult
      loop

compile :: String -> String -> Either CompileError (Program Resolved)
compile name source = do
  tokenized <- failIfError $ Token.tokenize name source
  parsed <- failIfError $ Term.parse name tokenized
  resolved <- Resolved.resolveProgram parsed
  void $ Typed.typeProgram resolved
  return resolved
  where failIfError = mapLeft $ CompileError . show

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
