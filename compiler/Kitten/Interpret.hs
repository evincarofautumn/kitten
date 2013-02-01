module Kitten.Interpret
  ( interpret
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Monoid
import Data.Vector (Vector, (!))

import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector

import Kitten.Def
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve

import qualified Kitten.Builtin as Builtin

data Stacks = Stacks
  { dataStack :: [Value]
  , localStack :: [Value]
  }

type StackIO = StateT Stacks IO

interpret :: [Value] -> Fragment Resolved -> IO [Value]
interpret stack (Fragment defs body)
  = liftM dataStack . execStateT (runTerm defs body) $ Stacks stack []

runTerm :: Vector (Def Resolved) -> Resolved -> StackIO ()
runTerm defs body = case body of
  Value value -> case value of
    Word (Name index) -> runDef $ defs ! index
    _ -> pushData value
  Builtin builtin -> case builtin of
    Builtin.At -> do
      Int b <- popData
      Vec a <- popData
      pushData $ a ! b
    Builtin.Bottom -> do
      Vec a <- popData
      pushData $ Vector.last a
    Builtin.Cat -> do
      Vec b <- popData
      Vec a <- popData
      pushData . Vec $ b <> a
    Builtin.Down -> do
      Vec a <- popData
      pushData . Vec $ Vector.tail a
    Builtin.Top -> do
      Vec a <- popData
      pushData $ Vector.head a
    Builtin.Up -> do
      Vec a <- popData
      pushData . Vec $ Vector.init a
    Builtin.Vec -> pushData . Vec . pure =<< popData
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
      recur a
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
    Builtin.Empty -> do
      Vec a <- popData
      pushData . Bool $ Vector.null a
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
      recur $ if condition then true else false
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
  Scoped term -> do
    pushLocal =<< popData
    recur term
    popLocal_
  Local (Name index) -> pushData =<< (!! index) <$> gets localStack
  Compose term1 term2 -> do
    recur term1
    recur term2
  Empty -> return ()
  where
  runDef (Def _ term) = recur term
  recur = runTerm defs

peekData :: StackIO Value
peekData = head <$> gets dataStack

popData :: StackIO Value
popData = do
  value <- head <$> gets dataStack
  popData_
  return value

popData_ :: StackIO ()
popData_ = modify $ \ s -> s { dataStack = tail $ dataStack s }

popLocal_ :: StackIO ()
popLocal_ = modify $ \ s -> s { localStack = tail $ localStack s }

pushData :: Value -> StackIO ()
pushData value = modify $ \ s -> s { dataStack = value : dataStack s }

pushLocal :: Value -> StackIO ()
pushLocal value = modify $ \ s -> s { localStack = value : localStack s }
