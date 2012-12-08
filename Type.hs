{-# LANGUAGE RecordWildCards #-}

module Type
  ( Type(..)
  , Typed(..)
  , typeProgram
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Function
import Data.Maybe
import Data.IntMap (IntMap)

import qualified Data.IntMap as IntMap

import Error
import Program
import Resolve (Resolved)
import Term (Term)

import qualified Resolve
import qualified Term

data Type
  = IntType
  | SVec Type Int
  | DVec Type
  | Type :> Type
  | Var Name
  | Type :. Type
  | EmptyType
  deriving (Eq, Ord)

infixl 4 :>
infixl 5 :.

data Typed
  = Word Int Type
  | Int Integer Type
  | Lambda String Typed Type
  | Vec [Typed] Type
  | Fun Typed Type
  | Compose Typed Typed Type
  | Empty Type

instance Show Typed where
  show (Word index type_)
    = "{@"
    ++ show index
    ++ " :: "
    ++ show type_
    ++ "}"
  show (Int value _)
    = show value
  show (Lambda name body type_)
    = "{"
    ++ "\\"
    ++ show name
    ++ " "
    ++ show body
    ++ " :: "
    ++ show type_
    ++ "}"
  show (Vec body type_)
    = "{("
    ++ unwords (map show body)
    ++ ") :: "
    ++ show type_
    ++ "}"
  show (Fun body type_)
    = "{["
    ++ show body
    ++ "] :: "
    ++ show type_
    ++ "}"
  show row = show' row
    where
    show' (Compose (Empty _) top _)
      = show top
    show' (Compose down top _)
      = show' down
      ++ " "
      ++ show top
    show' (Empty _) = ""
    show' scalar = show scalar

toTyped :: Resolved -> Inferred Typed
toTyped resolved = case resolved of
  Resolve.Word index -> Word index <$> fresh
  Resolve.Int value -> Int value <$> fresh
  Resolve.Lambda name term -> Lambda name <$> toTyped term <*> fresh
  Resolve.Vec terms -> Vec <$> mapM toTyped terms <*> fresh
  Resolve.Fun term -> Fun <$> toTyped term <*> fresh
  Resolve.Compose down top
    -> Compose <$> toTyped down <*> toTyped top <*> fresh
  Resolve.Empty -> Empty <$> fresh

instance Show Type where
  show IntType = "int"
  show (Var name) = show name
  show (SVec type_ size)
    = "(" ++ show type_ ++ "{" ++ show size ++ "})"
  show (DVec type_)
    = "(" ++ show type_ ++ "*)"
  show (a :> b)
    = "{" ++ show a ++ " -> " ++ show b ++ "}"
  show (EmptyType :. a) = show a
  show EmptyType = "()"
  show (a :. b) = show a ++ " " ++ show b

both f = f *** f

rowToList (a :. b) = b : rowToList a
rowToList a = [a]

rowFromList (a : b) = rowFromList b :. a
rowFromList [a] = a
rowFromList [] = EmptyType

newtype Name = Name Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name name) = 't' : show name

data Env = Env
  { next  :: Name
  , types :: IntMap.IntMap Type
  }

type Inferred = StateT Env (Either CompileError)

runInferred :: Inferred a -> Env -> Either CompileError (a, Env)
runInferred = runStateT

fresh :: Inferred Type
fresh = Var <$> freshName

freshName :: Inferred Name
freshName = do
  name@(Name x) <- gets next
  modify $ \ e -> e { next = Name $ succ x }
  return name

declareType :: Name -> Type -> Env -> Env
declareType (Name var) type_ env@Env{..} = env
  { types = IntMap.insert var type_ types }

typeProgram :: (Program Resolved) -> Either CompileError (Program Typed)
typeProgram (Program _defs term) = Program [] <$> typeTerm term

typeTerm :: Resolved -> Either CompileError Typed
typeTerm term = uncurry substTyped
  <$> runInferred inference typeEnv0
  where
  inference = do
    typed <- toTyped term
    typed <$ infer typed
  typeEnv0 = Env (Name 0) IntMap.empty

infer :: Typed -> Inferred Type
infer term = do
  r <- fresh
  case term of
    Word _index _type -> error "TODO: word type"
    Int _value type_ -> do
      let result = r :> r :. IntType
      unifyM type_ result
      return result
    Lambda _name _term _type -> error "TODO: lambda type"
    Compose x y type_ -> do
      (a :> b) <- infer x
      (c :> d) <- infer y
      unifyM b c
      let result = a :> d
      unifyM type_ result
      return result
    Vec terms type_ -> do
      termTypes <- mapM infer terms
      termType <- unifyEach termTypes
      let result = r :> r :. SVec termType (length terms)
      unifyM type_ result
      return result
      where
      unifyEach (x:y:zs) = unifyM x y >> unifyEach (y:zs)
      unifyEach [x] = return x
      unifyEach [] = fresh
    Fun x type_ -> do
      a <- infer x
      let result = r :> r :. a
      unifyM type_ result
      return result
    Empty type_ -> do
      let result = r :> r
      unifyM type_ result
      return result

unifyM :: Type -> Type -> Inferred ()
unifyM type1 type2 = do
  env <- gets $ unify type1 type2
  case env of
    Right env' -> put env'
    Left err -> lift . Left . CompileError
      $ "Unification error: " ++ show err

findType :: Name -> Env -> Either CompileError Type
findType (Name var) Env{..} = maybeToEither
  (CompileError $ "Nonexistent type variable " ++ show var ++ "!")
  $ IntMap.lookup var types

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

unify :: Type -> Type -> Env -> Either CompileError Env
unify a b env = (unify' `on` substChain env) a b env

unify' :: Type -> Type -> Env -> Either CompileError Env
unify' a b | a == b = return
unify' (SVec a m) (SVec b n) = unify a b >=> unifySize m n
unify' (SVec a _) (DVec b) = unify a b
unify' (DVec a) (SVec b _) = unify a b
unify' (DVec a) (DVec b) = unify a b
unify' (a :> b) (c :> d) = unify b d >=> unify a c
unify' (a :. b) (c :. d) = unify b d >=> unify a c
unify' (Var var) type_ = unifyVar var type_
unify' type_ (Var var) = unifyVar var type_
unify' a b = const . Left . CompileError $ unwords
  ["cannot solve type constraint:", show a, "=", show b]

unifySize :: Int -> Int -> Env -> Either CompileError Env
unifySize a b env
  | a == b = Right env
  | otherwise = Left . CompileError . unwords
    $ ["cannot solve size constraint:", show a, "=", show b]

unifyVar :: Name -> Type -> Env -> Either CompileError Env
unifyVar var1 (Var var2) env | var1 == var2 = return env
unifyVar var1 (Var var2) env
  = return $ declareType var1 (Var var2) env
unifyVar var type_ env | occurs var type_ env
  = Left . CompileError $ unwords
  ["cannot construct infinite type", show var, "=", show type_]
unifyVar var type_ env
  = return $ declareType var type_ env

occurs :: Name -> Type -> Env -> Bool
occurs _ IntType _ = False
occurs _ EmptyType _ = False
occurs var (SVec type_ _) env = occurs var type_ env
occurs var (DVec type_) env = occurs var type_ env
occurs var1 (Var var2) env = case findType var2 env of
  Left _ -> var1 == var2
  Right type_ -> occurs var1 type_ env
occurs var (a :> b) env = occurs var a env || occurs var b env
occurs var (a :. b) env = occurs var a env || occurs var b env

substChain :: Env -> Type -> Type
substChain env (Var var)
  | Right type_ <- findType var env = substChain env type_
substChain _ type_ = type_

substType :: Type -> Env -> Type
substType (a :> b) env = substType a env :> substType b env
substType (a :. b) env = substType a env :. substType b env
substType (Var var) env
  | Right type_ <- findType var env = substType type_ env
substType type_ _ = type_

substTyped :: Typed -> Env -> Typed
substTyped typed env = case typed of
  Word index type_ -> Word index $ substType type_ env
  Int value type_ -> Int value $ substType type_ env
  Lambda name body type_
    -> Lambda name (substTyped body env) (substType type_ env)
  Vec body type_ -> Vec (map (`substTyped` env) body) (substType type_ env)
  Fun body type_ -> Fun (substTyped body env) (substType type_ env)
  Compose down top type_
    -> Compose (substTyped down env) (substTyped top env) (substType type_ env)
  Empty type_ -> Empty $ substType type_ env
