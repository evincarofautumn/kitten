{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Function
import Data.List
import System.IO

import Def
import Error
import Name
import Program
import Resolve (Resolved)
import Term (Term)
import Type (Typed)

import qualified Builtin
import qualified Resolve as Resolved
import qualified Term
import qualified Type as Typed
import qualified Token

import Debug.Trace

main :: IO ()
main = fix $ \ loop -> do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      case compile "STDIN" line of
        Left compileError -> putStrLn $ show compileError
        Right compileResult -> runProgram compileResult
      loop

compile :: String -> String -> Either CompileError (Program Resolved)
compile name source = do
  tokenized <- failIfError $ Token.tokenize name source
  parsed <- failIfError $ Term.parse name tokenized
  resolved <- Resolved.resolveProgram parsed
  Typed.typeProgram resolved
  return resolved
  where failIfError = mapLeft $ CompileError . show

data Stacks = Stacks
  { dataStack :: [Resolved.Value]
  , localStack :: [Resolved.Value]
  }

(%) :: (a -> b) -> a -> b
(%) = ($)
infixl 0 %

runProgram :: Program Resolved -> IO ()
runProgram (Program defs term) = do
  Stacks {..} <- execStateT % runTerm term % Stacks [] []
  putStrLn . unwords . reverse $ map show dataStack
  where
  runTerm (Resolved.Value value) = case value of
    Resolved.Word (Name index) -> runDef $ defs !! index
    _ -> pushData value
  runTerm (Resolved.Builtin builtin) = case builtin of
    Builtin.Add -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a + b
    Builtin.AndBool -> do
      (Resolved.Bool b) <- popData
      (Resolved.Bool a) <- popData
      pushData . Resolved.Bool $ a && b
    Builtin.AndInt -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a .&. b
    Builtin.Apply -> do
      (Resolved.Fun a) <- popData
      runTerm a
    Builtin.Compose -> do
      (Resolved.Fun b) <- popData
      (Resolved.Fun a) <- popData
      pushData . Resolved.Fun $ Resolved.Compose a b
    Builtin.Div -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a `div` b
    Builtin.Drop -> popData_
    Builtin.Dup -> pushData =<< peekData
    Builtin.Eq -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a == b
    Builtin.Fun -> do
      a <- popData
      pushData . Resolved.Fun $ Resolved.Value a
    Builtin.Ge -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a >= b
    Builtin.Gt -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a > b
    Builtin.If -> error "TODO: run builtin if"
    Builtin.Le -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a <= b
    Builtin.Lt -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a < b
    Builtin.Mod -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a `mod` b
    Builtin.Mul -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a * b
    Builtin.Ne -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Bool $ a /= b
    Builtin.Neg -> do
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ negate a
    Builtin.NotBool -> do
      (Resolved.Bool a) <- popData
      pushData . Resolved.Bool $ not a
    Builtin.NotInt -> do
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ complement a
    Builtin.OrBool -> do
      (Resolved.Bool b) <- popData
      (Resolved.Bool a) <- popData
      pushData . Resolved.Bool $ a || b
    Builtin.OrInt -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a .|. b
    Builtin.Sub -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a - b
    Builtin.Swap -> do
      b <- popData
      a <- popData
      pushData b
      pushData a
    Builtin.XorBool -> do
      (Resolved.Bool b) <- popData
      (Resolved.Bool a) <- popData
      pushData . Resolved.Bool $ a /= b
    Builtin.XorInt -> do
      (Resolved.Int b) <- popData
      (Resolved.Int a) <- popData
      pushData . Resolved.Int $ a `xor` b
  runTerm (Resolved.Scoped term) = do
    pushLocal =<< popData
    runTerm term
    popLocal_
  runTerm (Resolved.Local (Name index))
    = pushData =<< (!! index) <$> gets localStack
  runTerm (Resolved.Compose term1 term2) = do
    runTerm term1
    runTerm term2
  runTerm Resolved.Empty = return ()
  runDef (Def _ term) = runTerm term

  pushData value = modify $ \ s -> s { dataStack = value : dataStack s }
  pushLocal value = modify $ \ s -> s { localStack = value : localStack s }
  peekData = head <$> gets dataStack
  popData_ = modify $ \ s -> s { dataStack = tail $ dataStack s }
  popData = do
    value <- head <$> gets dataStack
    popData_
    return value
  popLocal_ = modify $ \ s -> s { localStack = tail $ localStack s }

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
