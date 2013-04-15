{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

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
  Builtin.Add -> fail "TODO interpret builtin 'add'"
  Builtin.AndBool -> fail "TODO interpret builtin 'andbool'"
  Builtin.AndInt -> fail "TODO interpret builtin 'andint'"
  Builtin.Apply -> fail "TODO interpret builtin 'apply'"
  Builtin.At -> fail "TODO interpret builtin 'at'"
  Builtin.Bottom -> fail "TODO interpret builtin 'bottom'"
  Builtin.Cat -> fail "TODO interpret builtin 'cat'"
  Builtin.Compose -> fail "TODO interpret builtin 'compose'"
  Builtin.Div -> fail "TODO interpret builtin 'div'"
  Builtin.Down -> fail "TODO interpret builtin 'down'"
  Builtin.Drop -> fail "TODO interpret builtin 'drop'"
  Builtin.Dup -> fail "TODO interpret builtin 'dup'"
  Builtin.Eq -> fail "TODO interpret builtin 'eq'"
  Builtin.Empty -> fail "TODO interpret builtin 'empty'"
  Builtin.Fun -> fail "TODO interpret builtin 'fun'"
  Builtin.Ge -> fail "TODO interpret builtin 'ge'"
  Builtin.Gt -> fail "TODO interpret builtin 'gt'"
  Builtin.If -> fail "TODO interpret builtin 'if'"
  Builtin.Le -> fail "TODO interpret builtin 'le'"
  Builtin.Length -> fail "TODO interpret builtin 'length'"
  Builtin.Lt -> fail "TODO interpret builtin 'lt'"
  Builtin.Mod -> fail "TODO interpret builtin 'mod'"
  Builtin.Mul -> fail "TODO interpret builtin 'mul'"
  Builtin.Ne -> fail "TODO interpret builtin 'ne'"
  Builtin.Neg -> fail "TODO interpret builtin 'neg'"
  Builtin.NotBool -> fail "TODO interpret builtin 'notbool'"
  Builtin.NotInt -> fail "TODO interpret builtin 'notint'"
  Builtin.OrBool -> fail "TODO interpret builtin 'orbool'"
  Builtin.OrInt -> fail "TODO interpret builtin 'orint'"
  Builtin.Print -> do
    Text text <- popData
    lift $ putStr text
  Builtin.Sub -> fail "TODO interpret builtin 'sub'"
  Builtin.Swap -> fail "TODO interpret builtin 'swap'"
  Builtin.Top -> fail "TODO interpret builtin 'top'"
  Builtin.Up -> fail "TODO interpret builtin 'up'"
  Builtin.Vec -> fail "TODO interpret builtin 'vec'"
  Builtin.XorBool -> fail "TODO interpret builtin 'xorbool'"
  Builtin.XorInt -> fail "TODO interpret builtin 'xorint'"

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
