{-# LANGUAGE RecordWildCards #-}

module Type
  ( Type(..)
  , Typed(..)
  , typeProgram
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.IntMap (IntMap)
import Data.List

import qualified Data.IntMap as IntMap

import Def
import Error
import Name
import Program
import Resolve (Resolved)

import qualified Resolve

data Type
  = IntType
  | !Type :> !Type
  | !Type :. !Type
  | SVec !Type !Int
  | DVec !Type
  | EmptyType
  | Var !Name
  deriving (Eq, Ord)

infixl 4 :>
infixl 5 :.

data Typed
  = Word Name Type
  | Int Integer Type
  | Scoped Typed Type
  | Local Name Type
  | Vec [Typed] Type
  | Fun Typed Type
  | Compose Typed Typed Type
  | Empty Type

data TypeScheme = Forall [Name] Type

instance Show Typed where
  show (Word (Name index) type_)
    = "{@"
    ++ show index
    ++ " :: "
    ++ show type_
    ++ "}"
  show (Int value _)
    = show value
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
  show (Scoped term _)
    = show term
  show (Local (Name index) _)
    = "local"
    ++ show index
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

toTyped :: Resolved -> Inference Typed
toTyped resolved = case resolved of
  Resolve.Word index -> Word index <$> freshFunction
  Resolve.Int value -> Int value <$> freshFunction
  Resolve.Scoped term -> Scoped <$> toTyped term <*> freshFunction
  Resolve.Local index -> Local index <$> freshFunction
  Resolve.Vec terms -> Vec <$> mapM toTyped terms <*> freshFunction
  Resolve.Fun term -> Fun <$> toTyped term <*> freshFunction
  Resolve.Compose down top
    -> Compose <$> toTyped down <*> toTyped top <*> freshFunction
  Resolve.Empty -> Empty <$> freshFunction
  where freshFunction = (:>) <$> fresh <*> fresh

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

instance Show TypeScheme where
  show (Forall vars term)
    = "forall [" ++ unwords (map show vars) ++ "]. " ++ show term

data Env = Env
  { envNext  :: Name
  , envTypes :: IntMap Type
  , envLocals :: [TypeScheme]
  , envDefs :: [TypeScheme]
  } deriving (Show)

type Inference = StateT Env (Either CompileError)

fresh :: Inference Type
fresh = Var <$> freshName

freshName :: Inference Name
freshName = do
  name@(Name x) <- gets envNext
  modify $ \ env -> env { envNext = Name $ succ x }
  return name

declareType :: Name -> Type -> Env -> Env
declareType (Name var) type_ env@Env{..}
  = env { envTypes = IntMap.insert var type_ envTypes }

typeProgram :: Program Resolved -> Either CompileError (Program Typed)
typeProgram program
  = fmap (uncurry substProgram) . flip runStateT emptyEnv
  $ inferProgram =<< toTypedProgram program

emptyEnv :: Env
emptyEnv = Env (Name 0) IntMap.empty [] []

toTypedProgram :: Program Resolved -> Inference (Program Typed)
toTypedProgram (Program defs term)
  = Program <$> mapM toTypedDef defs <*> toTyped term

toTypedDef :: Def Resolved -> Inference (Def Typed)
toTypedDef (Def name term) = Def name <$> toTyped term

inferProgram :: Program Typed -> Inference (Program Typed)
inferProgram program@(Program defs term) = do
  defMap <- mapM inferDef defs
  modify $ \ env -> env { envDefs = defMap }
  void $ infer term
  gets $ substProgram program

inferDef :: Def Typed -> Inference TypeScheme
inferDef (Def _ term) = generalize $ infer term

infer :: Typed -> Inference Type
infer typedTerm = do
  r <- fresh
  case typedTerm of
    Word (Name index) _type
      -> unifyM _type =<< instantiate =<< gets ((!! index) . envDefs)
    Int _ type_
      -> unifyM type_ $ r :> r :. IntType
    Scoped (Fun term _) type_
      -> unifyM type_ =<< infer (Scoped term type_)
    -- Note the similarity to composition here.
    Scoped term type_ -> do
      a <- fresh
      local (Forall [] a) $ do
        (b :> c) <- infer term
        void $ unifyM r b
        unifyM type_ $ r :. a :> c
    Local (Name index) type_ -> do
      termType <- instantiate =<< gets ((!! index) . envLocals)
      unifyM type_ $ r :> r :. termType
    Compose x y type_ -> do
      (a :> b) <- infer x
      (c :> d) <- infer y
      void $ unifyM b c
      unifyM type_ $ a :> d
    Vec terms type_ -> do
      termTypes <- mapM infer terms
      termType <- unifyEach termTypes
      unifyM type_ $ r :> r :. SVec termType (length terms)
      where
      unifyEach (x:y:zs) = unifyM x y >> unifyEach (y:zs)
      unifyEach [x] = return x
      unifyEach [] = fresh
    Fun x type_ -> do
      a <- infer x
      unifyM type_ $ r :> r :. a
    Empty type_
      -> unifyM type_ $ r :> r
  where
  local scheme action = do
    modify $ \ env -> env { envLocals = envLocals env ++ [scheme] }
    result <- action
    modify $ \ env -> env { envLocals = init $ envLocals env }
    return result

unifyM :: Type -> Type -> Inference Type
unifyM type1 type2 = do
  env <- gets $ unify type1 type2
  case env of
    Right env' -> put env' >> return type2
    Left err -> lift . Left . CompileError
      $ "Unification error: " ++ show err

findType :: Env -> Name -> Either CompileError Type
findType Env{..} (Name var) = maybeToEither
  (CompileError $ "Nonexistent type variable " ++ show var ++ "!")
  $ IntMap.lookup var envTypes

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
occurs var1 (Var var2) env = case findType env var2 of
  Left _ -> var1 == var2
  Right type_ -> occurs var1 type_ env
occurs var (a :> b) env = occurs var a env || occurs var b env
occurs var (a :. b) env = occurs var a env || occurs var b env

substChain :: Env -> Type -> Type
substChain env (Var var)
  | Right type_ <- findType env var = substChain env type_
substChain _ type_ = type_

substProgram :: Program Typed -> Env -> Program Typed
substProgram (Program defs term) env
  = Program (map (substDef env) defs) (substTerm env term)

substDef :: Env -> Def Typed -> Def Typed
substDef env (Def name term) = Def name $ substTerm env term

substTerm :: Env -> Typed -> Typed
substTerm env typed = case typed of
  Word index type_ -> Word index $ substType env type_
  Int value type_ -> Int value $ substType env type_
  Scoped term type_ -> Scoped (substTerm env term) (substType env type_)
  Local index type_ -> Local index $ substType env type_
  Vec body type_ -> Vec (map (substTerm env) body) (substType env type_)
  Fun body type_ -> Fun (substTerm env body) (substType env type_)
  Compose down top type_
    -> Compose (substTerm env down) (substTerm env top) (substType env type_)
  Empty type_ -> Empty $ substType env type_

substType :: Env -> Type -> Type
substType env (a :> b) = substType env a :> substType env b
substType env (a :. b) = substType env a :. substType env b
substType env (Var var)
  | Right type_ <- findType env var = substType env type_
substType _ type_ = type_

instantiate :: TypeScheme -> Inference Type
instantiate (Forall vars type_) = do
  env <- rename vars
  return $ substType env type_
  where
  rename [] = return emptyEnv
  rename (v:vs) = do
    env <- rename vs
    a <- fresh
    return $ declareType v a env

generalize :: Inference Type -> Inference TypeScheme
generalize action = do
  before <- get
  type_ <- action
  after <- get
  let substituted = substType after type_
  let dependent = dependentBetween before after
  let vars = filter dependent . nub $ free substituted
  return $ Forall vars substituted

free :: Type -> [Name]
free IntType = []
free (a :> b) = free a ++ free b
free (a :. b) = free a ++ free b
free (SVec a _) = free a
free (DVec a) = free a
free (Var var) = [var]
free EmptyType = []

dependentBetween :: Env -> Env -> Name -> Bool
dependentBetween before after var1
  = any (bound after) (unbound before)
  where bound env var2 = occurs var1 (Var var2) env

unbound :: Env -> [Name]
unbound Env{..} = filter (\ (Name var) -> not $ IntMap.member var envTypes)
  [Name 0 .. pred envNext]
