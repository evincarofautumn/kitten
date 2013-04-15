{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck
  ( typecheck
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve (Resolved(..), Value(..))
import Kitten.Type

import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Type]
  , envLocals :: [Type]
  }

type TypecheckM a = StateT Env (Either CompileError) a
type Typecheck = TypecheckM ()

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either CompileError ()
typecheck _prelude stack Fragment{..} = do
  stackTypes <- mapM valueType stack
  evalStateT (typecheckTerm fragmentTerm) (Env stackTypes [])

typecheckTerm :: Resolved -> Typecheck
typecheckTerm resolved = case resolved of
  Value value -> typecheckValue value
  Builtin builtin -> typecheckBuiltin builtin
  Scoped term -> do
    pushLocal =<< popData
    typecheckTerm term
  Local (Name _name) -> compileError "TODO typecheck local"
  Compose terms -> mapM_ typecheckTerm terms

typecheckValue :: Value -> Typecheck
typecheckValue = pushData <=< lift . valueType

valueType :: Value -> Either CompileError Type
valueType value = case value of
  Word (Name _name) -> Left $ CompileError "TODO typecheck name"
  Int _ -> Right IntType
  Bool _ -> Right BoolType
  Text _ -> Right TextType
  Vec _values -> Left $ CompileError "TODO typecheck vec"
  Tuple _values -> Left $ CompileError "TODO typecheck tuple"
  Fun _term -> Left $ CompileError "TODO typecheck fun"

typecheckBuiltin :: Builtin -> Typecheck
typecheckBuiltin builtin = case builtin of
  Builtin.Add -> compileError "TODO typecheck builtin 'add'"
  Builtin.AndBool -> compileError "TODO typecheck builtin 'andbool'"
  Builtin.AndInt -> compileError "TODO typecheck builtin 'andint'"
  Builtin.Apply -> compileError "TODO typecheck builtin 'apply'"
  Builtin.At -> compileError "TODO typecheck builtin 'at'"
  Builtin.Bottom -> compileError "TODO typecheck builtin 'bottom'"
  Builtin.Cat -> compileError "TODO typecheck builtin 'cat'"
  Builtin.Compose -> compileError "TODO typecheck builtin 'compose'"
  Builtin.Div -> compileError "TODO typecheck builtin 'div'"
  Builtin.Down -> compileError "TODO typecheck builtin 'down'"
  Builtin.Drop -> compileError "TODO typecheck builtin 'drop'"
  Builtin.Dup -> compileError "TODO typecheck builtin 'dup'"
  Builtin.Eq -> compileError "TODO typecheck builtin 'eq'"
  Builtin.Empty -> compileError "TODO typecheck builtin 'empty'"
  Builtin.Fun -> compileError "TODO typecheck builtin 'fun'"
  Builtin.Ge -> compileError "TODO typecheck builtin 'ge'"
  Builtin.Gt -> compileError "TODO typecheck builtin 'gt'"
  Builtin.If -> compileError "TODO typecheck builtin 'if'"
  Builtin.Le -> compileError "TODO typecheck builtin 'le'"
  Builtin.Length -> compileError "TODO typecheck builtin 'length'"
  Builtin.Lt -> compileError "TODO typecheck builtin 'lt'"
  Builtin.Mod -> compileError "TODO typecheck builtin 'mod'"
  Builtin.Mul -> compileError "TODO typecheck builtin 'mul'"
  Builtin.Ne -> compileError "TODO typecheck builtin 'ne'"
  Builtin.Neg -> compileError "TODO typecheck builtin 'neg'"
  Builtin.NotBool -> compileError "TODO typecheck builtin 'notbool'"
  Builtin.NotInt -> compileError "TODO typecheck builtin 'notint'"
  Builtin.OrBool -> compileError "TODO typecheck builtin 'orbool'"
  Builtin.OrInt -> compileError "TODO typecheck builtin 'orint'"
  Builtin.Print -> popDataExpecting TextType
  Builtin.Sub -> compileError "TODO typecheck builtin 'sub'"
  Builtin.Swap -> compileError "TODO typecheck builtin 'swap'"
  Builtin.Top -> compileError "TODO typecheck builtin 'top'"
  Builtin.Up -> compileError "TODO typecheck builtin 'up'"
  Builtin.Vec -> compileError "TODO typecheck builtin 'vec'"
  Builtin.XorBool -> compileError "TODO typecheck builtin 'xorbool'"
  Builtin.XorInt -> compileError "TODO typecheck builtin 'xorint'"

compileError :: String -> TypecheckM a
compileError = lift . Left . CompileError

popData :: TypecheckM Type
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> compileError "stack underflow"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popDataExpecting :: Type -> Typecheck
popDataExpecting type_ = do
  dataStack <- gets envData
  case dataStack of
    [] -> compileError $ unwords
      ["expecting", show type_, "but got empty stack"]
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      unless (top == type_) . compileError $ unwords
        ["expecting", show type_, "but got", show top]

pushData :: Type -> Typecheck
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

pushLocal :: Type -> Typecheck
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }
