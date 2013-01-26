module Kitten.Interpret
  ( interpret
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Vector ((!))

import qualified Data.Text.IO as Text

import Kitten.Def
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve

import qualified Kitten.Builtin as Builtin

data Stacks = Stacks
  { dataStack :: [Value]
  , localStack :: [Value]
  }

interpret :: [Value] -> Fragment Resolved -> IO [Value]
interpret stack (Fragment defs body)
  = liftM dataStack . execStateT (runTerm body) $ Stacks stack []
  where
  runTerm (Value value) = case value of
    Word (Name index) -> runDef $ defs ! index
    _ -> pushData value
  runTerm (Builtin builtin) = case builtin of
    Builtin.Add -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a + b
    Builtin.AndBool -> do
      Bool b <- popData
      Bool a <- popData
      pushData . Bool $ a && b
    Builtin.AndInt -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a .&. b
    Builtin.Apply -> do
      Fun a <- popData
      runTerm a
    Builtin.Compose -> do
      Fun b <- popData
      Fun a <- popData
      pushData . Fun $ Compose a b
    Builtin.Div -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a `div` b
    Builtin.Drop -> popData_
    Builtin.Dup -> pushData =<< peekData
    Builtin.Eq -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a == b
    Builtin.Fun -> do
      a <- popData
      pushData . Fun $ Value a
    Builtin.Ge -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a >= b
    Builtin.Gt -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a > b
    Builtin.If -> do
      Bool condition <- popData
      Fun false <- popData
      Fun true <- popData
      runTerm $ if condition then true else false
    Builtin.Le -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a <= b
    Builtin.Lt -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a < b
    Builtin.Mod -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a `mod` b
    Builtin.Mul -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a * b
    Builtin.Ne -> do
      Int b <- popData
      Int a <- popData
      pushData . Bool $ a /= b
    Builtin.Neg -> do
      Int a <- popData
      pushData . Int $ negate a
    Builtin.NotBool -> do
      Bool a <- popData
      pushData . Bool $ not a
    Builtin.NotInt -> do
      Int a <- popData
      pushData . Int $ complement a
    Builtin.OrBool -> do
      Bool b <- popData
      Bool a <- popData
      pushData . Bool $ a || b
    Builtin.OrInt -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a .|. b
    Builtin.Print -> do
      Text s <- popData
      lift $ Text.putStr s
    Builtin.Sub -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a - b
    Builtin.Swap -> do
      b <- popData
      a <- popData
      pushData b
      pushData a
    Builtin.XorBool -> do
      Bool b <- popData
      Bool a <- popData
      pushData . Bool $ a /= b
    Builtin.XorInt -> do
      Int b <- popData
      Int a <- popData
      pushData . Int $ a `xor` b
  runTerm (Scoped term) = do
    pushLocal =<< popData
    runTerm term
    popLocal_
  runTerm (Local (Name index))
    = pushData =<< (!! index) <$> gets localStack
  runTerm (Compose term1 term2) = do
    runTerm term1
    runTerm term2
  runTerm Empty = return ()
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
