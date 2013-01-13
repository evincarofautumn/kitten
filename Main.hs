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
  { dataStack :: [Value]
  , localStack :: [Value]
  }

(%) :: (a -> b) -> a -> b
(%) = ($)
infixl 0 %

runProgram :: Program Resolved -> IO ()
runProgram (Program defs term) = do
  Stacks {..} <- execStateT % runTerm term % Stacks [] []
  putStrLn . unwords $ map show dataStack
  where
  runTerm (Resolved.Word (Name index)) = runDef $ defs !! index
  runTerm (Resolved.Builtin builtin) = case builtin of
    Builtin.Add -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a + b
    Builtin.AndBool -> do
      (Bool b) <- popData
      (Bool a) <- popData
      pushData . Bool $ a && b
    Builtin.AndInt -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a .&. b
    Builtin.Apply -> do
      (Fun a) <- popData
      mapM_ runTerm a
    Builtin.Compose -> do
      (Fun b) <- popData
      (Fun a) <- popData
      pushData . Fun $ a ++ b
    Builtin.Div -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a `div` b
    Builtin.Drop -> popData_
    Builtin.Dup -> pushData =<< peekData
    Builtin.Eq -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a == b
    Builtin.Fun -> error "TODO: run builtin fun"
    Builtin.Ge -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a >= b
    Builtin.Gt -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a > b
    Builtin.If -> error "TODO: run builtin if"
    Builtin.Le -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a <= b
    Builtin.Lt -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a < b
    Builtin.Mod -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a `mod` b
    Builtin.Mul -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a * b
    Builtin.Ne -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Bool $ a /= b
    Builtin.Neg -> do
      (Int a) <- popData
      pushData . Int $ negate a
    Builtin.NotBool -> do
      (Bool a) <- popData
      pushData . Bool $ not a
    Builtin.NotInt -> do
      (Int a) <- popData
      pushData . Int $ complement a
    Builtin.OrBool -> do
      (Bool b) <- popData
      (Bool a) <- popData
      pushData . Bool $ a || b
    Builtin.OrInt -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a .|. b
    Builtin.Sub -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a - b
    Builtin.Swap -> do
      b <- popData
      a <- popData
      pushData b
      pushData a
    Builtin.XorBool -> do
      (Bool b) <- popData
      (Bool a) <- popData
      pushData . Bool $ a /= b
    Builtin.XorInt -> do
      (Int b) <- popData
      (Int a) <- popData
      pushData . Int $ a `xor` b
  runTerm (Resolved.Int value) = pushData . Int $ fromInteger value
  runTerm (Resolved.Bool value) = pushData $ Bool value
  runTerm (Resolved.Scoped term) = do
    pushLocal =<< popData
    runTerm term
    popLocal_
  runTerm (Resolved.Local (Name index))
    = pushData =<< (!! index) <$> gets localStack
  runTerm (Resolved.Vec _terms)
    = error "TODO: run vec"
  runTerm (Resolved.Fun term) = pushData (Fun [term])
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

data Value
  = Int Int
  | Bool Bool
  | Vec [Value]
  | Fun [Resolved]

instance Show Value where
  show (Int value) = show value
  show (Bool value) = if value then "true" else "false"
  show (Vec values) = "[" ++ unwords (map show values) ++ "]"
  show (Fun _) = "{...}"

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a
