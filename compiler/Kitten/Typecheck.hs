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
import Kitten.Util.List

import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Type]
  , envLocals :: [Type]
  , envDefs :: [Def Resolved]
  }

type TypecheckM a = StateT Env (Either CompileError) a
type Typecheck = TypecheckM ()

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either CompileError ()
typecheck _prelude stack Fragment{..} = evalStateT
  (mapM_ typecheckValue stack >> typecheckTerm fragmentTerm)
  (Env [] [] fragmentDefs)

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
typecheckValue value = case value of
  Word (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> compileError $ "Internal name resolution error"
      Just (Def _ term) -> typecheckTerm term
  Int _ -> pushData IntType
  Bool _ -> pushData BoolType
  Text _ -> pushData TextType
  Vec _values -> compileError "TODO typecheck vec"
  Tuple _values -> compileError "TODO typecheck tuple"
  Fun _term -> compileError "TODO typecheck fun"

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
  Builtin.Drop -> popData_
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
  Builtin.Print -> popDataExpecting_ TextType
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
    popDataExpecting_ BoolType
    pushData BoolType

  boolsToBool = do
    popDataExpecting_ BoolType
    popDataExpecting_ BoolType
    pushData BoolType

  intToInt = do
    popDataExpecting_ IntType
    pushData IntType

  intsToInt = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData IntType

  intsToBool = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData BoolType

compileError :: String -> TypecheckM a
compileError = lift . Left . CompileError

popData_ :: Typecheck
popData_ = void popData

popData :: TypecheckM Type
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> compileError "expecting type but got empty stack"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popDataExpecting_ :: Type -> Typecheck
popDataExpecting_ = void . popDataExpecting

popDataExpecting :: Type -> TypecheckM Type
popDataExpecting type_ = do
  dataStack <- gets envData
  case dataStack of
    [] -> compileError $ unwords
      ["expecting", show type_, "but got empty stack"]
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      unless (top == type_) . compileError $ unwords
        ["expecting", show type_, "but got", show top]
      return top

pushData :: Type -> Typecheck
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

pushLocal :: Type -> Typecheck
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }
