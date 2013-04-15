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
  Builtin.Add -> intsToInt
  Builtin.AndBool -> boolsToBool
  Builtin.AndInt -> intsToInt
  Builtin.Apply -> compileError "TODO typecheck builtin 'apply'"
  Builtin.At -> compileError "TODO typecheck builtin 'at'"
  Builtin.Bottom -> compileError "TODO typecheck builtin 'bottom'"
  Builtin.Cat -> compileError "TODO typecheck builtin 'cat'"
  Builtin.Compose -> compileError "TODO typecheck builtin 'compose'"
  Builtin.Div -> intsToInt
  Builtin.Down -> compileError "TODO typecheck builtin 'down'"
  Builtin.Drop -> void popData
  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a
  Builtin.Eq -> intsToBool
  Builtin.Empty -> compileError "TODO typecheck builtin 'empty'"
  Builtin.Fun -> compileError "TODO typecheck builtin 'fun'"
  Builtin.Ge -> intsToBool
  Builtin.Gt -> intsToBool
  Builtin.If -> compileError "TODO typecheck builtin 'if'"
  Builtin.Le -> intsToBool
  Builtin.Length -> compileError "TODO typecheck builtin 'length'"
  Builtin.Lt -> intsToBool
  Builtin.Mod -> intsToInt
  Builtin.Mul -> intsToInt
  Builtin.Ne -> intsToBool
  Builtin.Neg -> intToInt
  Builtin.NotBool -> boolToBool
  Builtin.NotInt -> intToInt
  Builtin.OrBool -> boolsToBool
  Builtin.OrInt -> intsToInt
  Builtin.Print -> popDataExpecting TextType
  Builtin.Sub -> intsToInt
  Builtin.Swap -> do
    a <- popData
    b <- popData
    pushData a
    pushData b
  Builtin.Top -> compileError "TODO typecheck builtin 'top'"
  Builtin.Up -> compileError "TODO typecheck builtin 'up'"
  Builtin.Vec -> compileError "TODO typecheck builtin 'vec'"
  Builtin.XorBool -> boolsToBool
  Builtin.XorInt -> intsToInt

  where

  boolToBool = do
    popDataExpecting BoolType
    pushData BoolType

  boolsToBool = do
    popDataExpecting BoolType
    popDataExpecting BoolType
    pushData BoolType

  intToInt = do
    popDataExpecting IntType
    pushData IntType

  intsToInt = do
    popDataExpecting IntType
    popDataExpecting IntType
    pushData IntType

  intsToBool = do
    popDataExpecting IntType
    popDataExpecting IntType
    pushData BoolType

compileError :: String -> TypecheckM a
compileError = lift . Left . CompileError

popData :: TypecheckM Type
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> compileError "expecting type but got empty stack"
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
