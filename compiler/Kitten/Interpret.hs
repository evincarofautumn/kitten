{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bits

import Kitten.Builtin (Builtin)
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve (Resolved(..), Value(..))

import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Value]
  , envLocals :: [Value]
  }

type InterpretM a = StateT Env IO a
type Interpret = InterpretM ()

interpret :: [Value] -> Fragment Resolved -> IO ()
interpret stack Fragment{..} = void $ evalStateT
  (interpretTerm fragmentTerm)
  (Env stack [])

interpretTerm :: Resolved -> Interpret
interpretTerm resolved = case resolved of
  Value value -> pushData value
  Builtin builtin -> interpretBuiltin builtin
  Scoped _term -> pushLocal =<< popData
  Local (Name _name) -> fail "TODO interpret locals"
  Compose terms -> mapM_ interpretTerm terms

interpretBuiltin :: Builtin -> Interpret
interpretBuiltin builtin = case builtin of
  Builtin.Add -> intsToInt (+)
  Builtin.AndBool -> boolsToBool (&&)
  Builtin.AndInt -> intsToInt (.&.)
  Builtin.Apply -> fail "TODO interpret builtin 'apply'"
  Builtin.At -> fail "TODO interpret builtin 'at'"
  Builtin.Bottom -> fail "TODO interpret builtin 'bottom'"
  Builtin.Cat -> fail "TODO interpret builtin 'cat'"
  Builtin.Compose -> fail "TODO interpret builtin 'compose'"
  Builtin.Div -> intsToInt div
  Builtin.Down -> fail "TODO interpret builtin 'down'"
  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.Eq -> intsToBool (==)
  Builtin.Empty -> do
    Vec a <- popData
    pushData . Bool $ null a

  Builtin.Fun -> fail "TODO interpret builtin 'fun'"

  Builtin.Ge -> intsToBool (>=)
  Builtin.Gt -> intsToBool (>)
  Builtin.If -> do
    Bool condition <- popData
    Fun false <- popData
    Fun true <- popData
    if condition
      then interpretTerm true
      else interpretTerm false

  Builtin.Le -> intsToBool (<=)
  Builtin.Length -> fail "TODO interpret builtin 'length'"
  Builtin.Lt -> intsToBool (<)
  Builtin.Mod -> intsToInt mod
  Builtin.Mul -> intsToInt (*)
  Builtin.Ne -> intsToBool (/=)
  Builtin.Neg -> intToInt negate
  Builtin.NotBool -> boolToBool not
  Builtin.NotInt -> intToInt complement
  Builtin.OrBool -> boolsToBool (||)
  Builtin.OrInt -> intsToInt (.|.)

  Builtin.Print -> do
    Text text <- popData
    lift $ putStr text

  Builtin.Sub -> intsToInt (-)
  Builtin.Swap -> do
    b <- popData
    a <- popData
    pushData b
    pushData a

  Builtin.Top -> fail "TODO interpret builtin 'top'"
  Builtin.Up -> fail "TODO interpret builtin 'up'"
  Builtin.Vec -> fail "TODO interpret builtin 'vec'"
  Builtin.XorBool -> boolsToBool (/=)
  Builtin.XorInt -> intsToInt xor

  where

  boolToBool :: (Bool -> Bool) -> Interpret
  boolToBool f = do
    Bool a <- popData
    pushData . Bool $ f a

  boolsToBool :: (Bool -> Bool -> Bool) -> Interpret
  boolsToBool f = do
    Bool b <- popData
    Bool a <- popData
    pushData . Bool $ f a b

  intsToBool :: (Int -> Int -> Bool) -> Interpret
  intsToBool f = do
    Int b <- popData
    Int a <- popData
    pushData . Bool $ f a b

  intToInt :: (Int -> Int) -> Interpret
  intToInt f = do
    Int a <- popData
    pushData . Int $ f a

  intsToInt :: (Int -> Int -> Int) -> Interpret
  intsToInt f = do
    Int b <- popData
    Int a <- popData
    pushData . Int $ f a b

pushData :: Value -> Interpret
pushData value = modify $ \ env@Env{..}
  -> env { envData = value : envData }

popData :: InterpretM Value
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> fail "stack underflow"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

pushLocal :: Value -> Interpret
pushLocal value = modify $ \ env@Env{..}
  -> env { envLocals = value : envLocals }
