{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type
  ( Type(..)
  , Typed(..)
  , manifestType
  , typeFragment
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.IntMap (IntMap)
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector, (!))

import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve (Resolved)
import Kitten.Util

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolve as Resolve

------------------------------------------------------------

data Type
  = BoolType
  | IntType
  | TextType
  | !Type :> !Type
  | !Type :. !Type
  | SVec !Type !Int
  | DVec !Type
  | TupleType !(Vector Type)
  | EmptyType
  | Var !Name
  deriving (Eq, Ord)

infixl 4 :>
infixl 5 :.

data Typed
  = Value !Value
  | Builtin !Builtin !Type
  | Scoped !Typed !Type
  | Local !Name !Type
  | Compose !Typed !Typed !Type
  | Empty !Type

data Value
  = Word !Name !Type
  | Int !Int !Type
  | Bool !Bool !Type
  | Text !Text !Type
  | Vec !(Vector Value) !Type
  | Tuple !(Vector Value) !Type
  | Fun !Typed !Type

data TypeScheme = Forall [Name] Type

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show TextType = "text"
  show (Var name) = show name
  show (SVec type_ size)
    = show type_ ++ "[" ++ show size ++ "]"
  show (DVec type_)
    = show type_ ++ "*"
  show (TupleType types)
    = "(" ++ unwords (map show $ Vector.toList types) ++ ")"
  show (a :> b)
    = "(" ++ show a ++ " -> " ++ show b ++ ")"
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
  , envDefs :: Vector TypeScheme
  }

type Inference = StateT Env (Either CompileError)

------------------------------------------------------------

-- | Runs type inference on a resolved AST.
typeFragment
  :: Vector (Def Resolved)
  -> [Resolve.Value]
  -> Fragment Resolved
  -> Either CompileError (Fragment Typed)
typeFragment prelude stack fragment
  = fmap (uncurry substFragment) . flip runStateT emptyEnv $ do
    typedPrelude <- Vector.mapM toTypedDef prelude
    typedFragment <- toTypedFragment fragment
    inferFragment stack typedPrelude typedFragment

-- | Annotates a resolved AST with fresh type variables.
toTypedFragment :: Fragment Resolved -> Inference (Fragment Typed)
toTypedFragment (Fragment defs term)
  = Fragment <$> Vector.mapM toTypedDef defs <*> toTyped term

-- | Annotates a resolved definition with fresh type variables.
toTypedDef :: Def Resolved -> Inference (Def Typed)
toTypedDef (Def name term) = Def name <$> toTyped term

-- | Annotates a resolved term with fresh type variables.
toTyped :: Resolved -> Inference Typed
toTyped resolved = case resolved of
  Resolve.Value value -> Value <$> toTypedValue value
  Resolve.Builtin name -> Builtin name <$> freshFunction
  Resolve.Scoped term -> Scoped <$> toTyped term <*> freshFunction
  Resolve.Local index -> Local index <$> freshFunction
  Resolve.Compose down top
    -> Compose <$> toTyped down <*> toTyped top <*> freshFunction
  Resolve.Empty -> Empty <$> freshFunction
  where
  toTypedValue v = case v of
    Resolve.Word index -> Word index <$> freshFunction
    Resolve.Int value -> Int value <$> freshFunction
    Resolve.Bool value -> Bool value <$> freshFunction
    Resolve.Text value -> Text value <$> freshFunction
    Resolve.Vec terms -> Vec
      <$> Vector.mapM toTypedValue terms <*> freshFunction
    Resolve.Fun term -> Fun <$> toTyped term <*> freshFunction
    Resolve.Tuple terms -> Tuple
      <$> Vector.mapM toTypedValue terms <*> freshFunction

------------------------------------------------------------

-- | Infers and annotates the type of a program fragment.
inferFragment
  :: [Resolve.Value]
  -> Vector (Def Typed)
  -> Fragment Typed
  -> Inference (Fragment Typed)
inferFragment stack prelude fragment@Fragment{..}
  = inferAllDefs >> inferTerm >> gets (substFragment fragment)
  where
  inferAllDefs = do
    initial <- Vector.replicateM
      (Vector.length prelude + Vector.length fragmentDefs) freshFunction
    setDefs $ Vector.map (Forall []) initial
    inferred <- (<>) <$> inferDefs prelude <*> inferDefs fragmentDefs
    Vector.forM_ (Vector.zip initial inferred) $ \ (var, scheme) -> do
      type_ <- instantiate scheme
      unifyM_ var type_
    setDefs inferred
  inferTerm = do
    (a :> b) <- infer <=< toTyped $ stackTerm stack
    (c :> _) <- infer fragmentTerm
    s <- fresh
    unifyM_ a (EmptyType :> s)
    unifyM_ b c
  inferDefs = Vector.mapM inferDef
  stackTerm = foldr (flip Resolve.Compose . Resolve.Value) Resolve.Empty
  setDefs ds = modify $ \ env@Env{..} -> env { envDefs = ds }

-- | Infers the type scheme of a definition.
inferDef :: Def Typed -> Inference TypeScheme
inferDef (Def _ term) = generalize $ infer term

-- | Infers the type of a term.
infer :: Typed -> Inference Type
infer typedTerm = do
  r <- fresh
  let
    boolToBool  = return $ r :. BoolType             :> r :. BoolType
    boolsToBool = return $ r :. BoolType :. BoolType :> r :. BoolType
    intToInt    = return $ r :. IntType              :> r :. IntType
    intsToBool  = return $ r :. IntType :. IntType   :> r :. BoolType
    intsToInt   = return $ r :. IntType :. IntType   :> r :. IntType

  case typedTerm of
    Value value -> case value of
      Word (Name index) _type
        -> unifyM _type =<< instantiate =<< gets ((! index) . envDefs)
      Int _ type_
        -> unifyM type_ $ r :> r :. IntType
      Bool _ type_
        -> unifyM type_ $ r :> r :. BoolType
      Text _ type_
        -> unifyM type_ $ r :> r :. TextType
      Vec terms type_ -> do
        termTypes <- mapM (infer . Value) $ Vector.toList terms
        termType <- unifyEach termTypes
        unifyM type_ $ r :> r :. SVec termType (Vector.length terms)
        where
        unifyEach (x:y:zs) = unifyM x y >> unifyEach (y:zs)
        unifyEach [x] = return x
        unifyEach [] = fresh
      Tuple terms type_ -> do
        termTypes <- Vector.mapM (infer . Value) terms
        unifyM type_ $ r :> r :. TupleType termTypes
      Fun x type_ -> do
        a <- infer x
        unifyM type_ $ r :> r :. a
    -- Note the similarity to composition here.
    Scoped term type_ -> do
      a <- fresh
      local (Forall [] a) $ do
        (b :> c) <- infer term
        unifyM_ r b
        unifyM type_ $ r :. a :> c
    Local (Name index) type_ -> do
      termType <- instantiate =<< gets ((!! index) . envLocals)
      unifyM type_ $ r :> r :. termType
    Compose x y type_ -> do
      (a :> b) <- infer x
      (c :> d) <- infer y
      unifyM_ b c
      unifyM type_ $ a :> d
    Empty type_ -> unifyM type_ $ r :> r
    Builtin name type_ -> unifyM type_ =<< case name of
      Builtin.Print -> return $ r :. TextType :> r
      Builtin.Dup
        -> (\ a -> r :. a :> r :. a :. a)
        <$> fresh
      Builtin.Swap
        -> (\ a b -> r :. a :. b :> r :. b :. a)
        <$> fresh <*> fresh
      Builtin.Drop
        -> (\ a -> r :. a :> r)
        <$> fresh
      Builtin.Fun
        -> (\ s a -> r :. a :> r :. (s :> s :. a))
        <$> fresh <*> fresh
      Builtin.Apply
        -> (\ s -> r :. (r :> s) :> s)
        <$> fresh
      Builtin.Compose
        -> (\ a b c -> r :. (a :> b) :. (b :> c) :> r :. (a :> c))
        <$> fresh <*> fresh <*> fresh
      Builtin.If
        -> (\ s -> r :. (r :> s) :. (r :> s) :. BoolType :> s)
        <$> fresh
      Builtin.AndBool -> boolsToBool
      Builtin.AndInt  -> intsToInt
      Builtin.OrBool  -> boolsToBool
      Builtin.OrInt   -> intsToInt
      Builtin.XorBool -> boolsToBool
      Builtin.XorInt  -> intsToInt
      Builtin.NotBool -> boolToBool
      Builtin.NotInt  -> intToInt
      Builtin.Add     -> intsToInt
      Builtin.Sub     -> intsToInt
      Builtin.Mul     -> intsToInt
      Builtin.Div     -> intsToInt
      Builtin.Mod     -> intsToInt
      Builtin.Neg     -> intToInt
      Builtin.Eq      -> intsToBool
      Builtin.Ne      -> intsToBool
      Builtin.Lt      -> intsToBool
      Builtin.Gt      -> intsToBool
      Builtin.Le      -> intsToBool
      Builtin.Ge      -> intsToBool
  where
  local scheme action = do
    modify $ \ env -> env { envLocals = envLocals env ++ [scheme] }
    result <- action
    modify $ \ env -> env { envLocals = init $ envLocals env }
    return result

------------------------------------------------------------

-- | Simplifies and unifies two types.
unify :: Type -> Type -> Env -> Either CompileError Env
unify a b env = (unify' `on` substChain env) a b env

-- | Unifies two simplified types.
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
unify' a b = const . Left . CompileError $ Text.unwords
  [ "cannot solve type constraint:"
  , textShow a
  , "="
  , textShow b
  ]

-- | Unifies two types.
unifyM_ :: Type -> Type -> Inference ()
unifyM_ = (void .) . unifyM

-- | Unifies two types, returning the second type.
unifyM :: Type -> Type -> Inference Type
unifyM type1 type2 = do
  env <- gets $ unify type1 type2
  case env of
    Right env' -> put env' >> return type2
    Left err -> lift . Left . CompileError
      $ "Unification error: " <> textShow err

-- | Unifies a variable with a type.
unifyVar :: Name -> Type -> Env -> Either CompileError Env
unifyVar var1 (Var var2) env | var1 == var2 = return env
unifyVar var1 (Var var2) env
  = return $ declareType var1 (Var var2) env
unifyVar var type_ env | occurs var type_ env
  = Left . CompileError $ Text.unwords
    ["cannot construct infinite type", varText, "=", typeText]
  where
  varText = textShow $ substType env (Var var)
  typeText = textShow $ substType env type_
unifyVar var type_ env
  = return $ declareType var type_ env

-- | Unifies two constant size constraints.
unifySize :: Int -> Int -> Env -> Either CompileError Env
unifySize a b env
  | a == b = Right env
  | otherwise = Left . CompileError . Text.unwords
    $ ["cannot solve size constraint:", textShow a, "=", textShow b]

------------------------------------------------------------

-- | Simplifies a chain of variable equality constraints.
substChain :: Env -> Type -> Type
substChain env (Var var)
  | Right type_ <- findType env var = substChain env type_
substChain _ type_ = type_

-- | Substitutes type variables in a program fragment.
substFragment :: Fragment Typed -> Env -> Fragment Typed
substFragment (Fragment defs term) env
  = Fragment (Vector.map (substDef env) defs) (substTerm env term)

-- | Substitutes type variables in a definition.
substDef :: Env -> Def Typed -> Def Typed
substDef env (Def name term) = Def name $ substTerm env term

-- | Substitutes type variables in a term.
substTerm :: Env -> Typed -> Typed
substTerm env typed = case typed of
  Value value -> Value $ substValue env value
  Builtin builtin type_ -> Builtin builtin $ substType env type_
  Scoped term type_ -> Scoped (substTerm env term) (substType env type_)
  Local index type_ -> Local index $ substType env type_
  Compose down top type_
    -> Compose (substTerm env down) (substTerm env top) (substType env type_)
  Empty type_ -> Empty $ substType env type_

substValue :: Env -> Value -> Value
substValue env v = case v of
  Word index type_ -> Word index $ substType env type_
  Int value type_ -> Int value $ substType env type_
  Bool value type_ -> Bool value $ substType env type_
  Text value type_ -> Text value $ substType env type_
  Vec values type_ -> Vec
    (Vector.map (substValue env) values) (substType env type_)
  Tuple values type_ -> Tuple
    (Vector.map (substValue env) values) (substType env type_)
  Fun body type_ -> Fun (substTerm env body) (substType env type_)

-- | Substitutes type variables in a type.
substType :: Env -> Type -> Type
substType env (a :> b) = substType env a :> substType env b
substType env (a :. b) = substType env a :. substType env b
substType env (Var var)
  | Right type_ <- findType env var = substType env type_
substType _ type_ = type_

------------------------------------------------------------

-- | Instantiates a type scheme in the current environment.
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

-- | Generalizes a type into a type scheme.
generalize :: Inference Type -> Inference TypeScheme
generalize action = do
  before <- get
  type_ <- action
  after <- get
  let substituted = substType after type_
  let dependent = dependentBetween before after
  let vars = filter dependent . nub $ free substituted
  return $ Forall vars substituted

-- | Enumerates free variables of a type.
free :: Type -> [Name]
free IntType = []
free BoolType = []
free TextType = []
free (a :> b) = free a ++ free b
free (a :. b) = free a ++ free b
free (SVec a _) = free a
free (DVec a) = free a
free (TupleType types) = free =<< Vector.toList types
free (Var var) = [var]
free EmptyType = []

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween :: Env -> Env -> Name -> Bool
dependentBetween before after var1
  = any (bound after) (unbound before)
  where bound env var2 = occurs var1 (Var var2) env

-- | Enumerates those type variables in an environment which
-- are allocated but not yet bound to a type.
unbound :: Env -> [Name]
unbound Env{..} = filter (\ (Name var) -> not $ IntMap.member var envTypes)
  [Name 0 .. pred envNext]

------------------------------------------------------------

-- | Declares a type variable.
declareType :: Name -> Type -> Env -> Env
declareType (Name var) type_ env@Env{..}
  = env { envTypes = IntMap.insert var type_ envTypes }

-- | Finds the type associated with a type variable name.
findType :: Env -> Name -> Either CompileError Type
findType Env{..} (Name var) = maybeToEither
  (CompileError $ "Nonexistent type variable " <> textShow var <> "!")
  $ IntMap.lookup var envTypes

-- | The famed "occurs check".
occurs :: Name -> Type -> Env -> Bool
occurs _ IntType _ = False
occurs _ BoolType _ = False
occurs _ TextType _ = False
occurs _ EmptyType _ = False
occurs var (SVec type_ _) env = occurs var type_ env
occurs var (DVec type_) env = occurs var type_ env
occurs var (TupleType types) env
  = Vector.any (\ type_ -> occurs var type_ env) types
occurs var1 (Var var2) env = case findType env var2 of
  Left _ -> var1 == var2
  Right type_ -> occurs var1 type_ env
occurs var (a :> b) env = occurs var a env || occurs var b env
occurs var (a :. b) env = occurs var a env || occurs var b env

-- | Generates a fresh type variable.
fresh :: Inference Type
fresh = Var <$> freshName
  where
  freshName = do
    name@(Name x) <- gets envNext
    modify $ \ env -> env { envNext = Name $ succ x }
    return name

freshFunction :: Inference Type
freshFunction = (:>) <$> fresh <*> fresh

emptyEnv :: Env
emptyEnv = Env (Name 0) IntMap.empty [] Vector.empty

------------------------------------------------------------

manifestType :: Typed -> Type
manifestType term = case term of
  Value value -> manifestValueType value
  Builtin _ type_ -> type_
  Scoped _ type_ -> type_
  Local _ type_ -> type_
  Compose _ _ type_ -> type_
  Empty type_ -> type_
  where
  manifestValueType value = case value of
    Word _ type_ -> type_
    Int _ type_ -> type_
    Bool _ type_ -> type_
    Text _ type_ -> type_
    Vec _ type_ -> type_
    Fun _ type_ -> type_
    Tuple _ type_ -> type_
