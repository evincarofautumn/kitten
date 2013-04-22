{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck
  ( typecheck
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List

import qualified Data.Foldable as Foldable

import Kitten.Anno (Anno(..))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolve (Resolved(..), Value(..))
import Kitten.Type
import Kitten.Util.List

import qualified Kitten.Anno as Anno
import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Type Scalar]
  , envLocals :: [Type Scalar]
  , envAnnos :: [Anno]
  , envDefs :: [Def Resolved]
  , envScalarVars :: [(Name, Type Scalar)]
  , envRowVars :: [(Name, Type Row)]
  , envNext :: Name
  , envHypothetical :: Bool
  , envLocations :: [Location]
  }

type TypecheckM a = StateT Env (Either CompileError) a
type Typecheck = TypecheckM ()

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either CompileError ()
typecheck _prelude stack Fragment{..}
  = flip evalStateT emptyEnv
  { envAnnos = fragmentAnnos
  , envDefs = fragmentDefs
  } $ do
    mapM_ typecheckValue stack
    mapM_ typecheckDef fragmentDefs
    mapM_ typecheckTerm fragmentTerms

typecheckDef :: Def Resolved -> Typecheck
typecheckDef Def{..} = withLocation defLocation $ do
  mAnno <- gets
    $ find ((defName ==) . Anno.annoName) . envAnnos
  case mAnno of
    Nothing -> typeError $ concat
      [ "missing type annotation for '"
      , defName
      , "'"
      ]
    Just anno -> void . hypothetically $ typecheckAnno anno

emptyEnv :: Env
emptyEnv = Env
  { envData = []
  , envLocals = []
  , envAnnos = []
  , envDefs = []
  , envScalarVars = []
  , envRowVars = []
  , envNext = Name 0
  , envHypothetical = False
  , envLocations = []
  }

freshName :: TypecheckM Name
freshName = do
  Name current <- gets envNext
  modify $ \ env@Env{..} -> env
    { envNext = Name (succ current) }
  return $ Name current

typecheckTerm :: Resolved -> Typecheck
typecheckTerm resolved = case resolved of
  Push value loc -> withLocation loc $ typecheckValue value
  Builtin builtin loc -> withLocation loc
    $ typecheckBuiltin builtin
  Scoped terms -> do
    pushLocal =<< popData
    mapM_ typecheckTerm terms
  Local name loc -> withLocation loc
    $ pushData =<< getLocal name
  Closed{} -> internalError "TODO typecheck closed"
  Compose terms -> mapM_ typecheckTerm terms

typecheckValue :: Value -> Typecheck
typecheckValue value = case value of
  Word (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> internalError
        "unresolved name appeared during type inference"
      Just (Def name _ _) -> do
        mAnno <- gets
          $ find ((name ==) . Anno.annoName) . envAnnos
        case mAnno of
          Nothing -> typeError $ concat
            [ "missing type annotation for '"
            , name
            , "'"
            ]
          Just anno -> typecheckAnno anno

  Int _ -> pushData IntType
  Bool _ -> pushData BoolType
  Text _ -> pushData TextType
  Vec [] -> do
    a <- freshName
    pushData $ VecType (ScalarVar a)
  Vec vs@(_ : _) -> do
    mapM_ typecheckValue vs
    (expected : rest) <- replicateM (length vs) popData
    forM_ rest $ unify expected
    pushData $ VecType expected
  Tuple _values -> internalError "TODO typecheck tuple"
  Fun _terms -> do
    a <- freshName
    b <- freshName
    pushData $ RowVar a :> RowVar b
  Closure{} -> internalError
    "closures should not appear during inference"
  Closure'{} -> internalError
    "closures should not appear during inference"

typecheckAnno :: Anno -> Typecheck
typecheckAnno anno = do
  type_ <- instantiate $ fromAnno anno
  case type_ of
    Composition as :> Composition bs -> do
      mapM_ popDataExpecting_ as
      mapM_ pushData bs
    _ -> internalError
      "TODO typecheck non-function type annotations"

instantiate :: Scheme Scalar -> TypecheckM (Type Scalar)
instantiate (Forall vars type_) = do
  next <- gets envNext
  let
    substitution
      = flip execStateT emptyEnv { envNext = next }
      $ Foldable.mapM_ rename vars
  substitution' <- lift substitution
  modify $ \ env -> env { envNext = envNext substitution' }
  return $ substScalarType substitution' type_
  where
  rename var = declareScalar var . ScalarVar <$> freshName

-- | Simplifies a chain of transitive equality constraints.
substChain :: Env -> Type Scalar -> Type Scalar
substChain env (ScalarVar var)
  | Right type_ <- lookupScalar env var
  = substChain env type_
substChain _ type_ = type_

substScalarType :: Env -> Type Scalar -> Type Scalar
substScalarType env (a :> b)
  = substRowType env a :> substRowType env b
substScalarType env (ScalarVar name)
  | Right type_ <- lookupScalar env name
  = substScalarType env type_
substScalarType _ type_ = type_

substRowType :: Env -> Type Row -> Type Row
substRowType env (Composition as)
  = Composition $ map (substScalarType env) as
substRowType env (RowVar name)
  | Right type_ <- lookupRow env name
  = substRowType env type_
substRowType _ type_ = type_

stackEffect :: TypecheckM a -> TypecheckM (Type Scalar)
stackEffect action = do
  (before, after) <- hypothetically action
  let
    stackBefore = envData before
    stackAfter = envData after
    (consumption, production)
      = stripCommonPrefix (reverse stackBefore) (reverse stackAfter)
  return
    $ Composition (reverse consumption)
    :> Composition (reverse production)

typecheckBuiltin :: Builtin -> Typecheck
typecheckBuiltin builtin = case builtin of
  Builtin.Add -> intsToInt

  Builtin.AndBool -> boolsToBool

  Builtin.AndInt -> intsToInt

  Builtin.Apply -> do
    a <- freshName
    b <- freshName
    (Composition consumption :> Composition production)
      <- popDataExpecting $ RowVar a :> RowVar b
    mapM_ popDataExpecting_ consumption
    mapM_ pushData production

  Builtin.At -> do
    popDataExpecting_ IntType
    x <- freshName
    VecType a <- popDataExpecting $ VecType (ScalarVar x)
    pushData a

  Builtin.Bottom -> do
    x <- freshName
    VecType a <- popDataExpecting $ VecType (ScalarVar x)
    pushData a

  Builtin.Cat -> do
    x <- freshName
    VecType b <- popDataExpecting $ VecType (ScalarVar x)
    VecType a <- popDataExpecting $ VecType (ScalarVar x)
    if a == b
      then pushData $ VecType a
      else typeError $ concat
        [ "Mismatched element types "
        , show a
        , " and "
        , show b
        , " in 'cat' arguments"
        ]

  Builtin.Compose -> do
    w <- freshName
    x <- freshName
    y <- freshName
    z <- freshName
    Composition c :> Composition d
      <- popDataExpecting $ RowVar y :> RowVar z
    Composition a :> Composition b
      <- popDataExpecting $ RowVar w :> RowVar x
    result <- stackEffect $ do
      mapM_ popDataExpecting a
      mapM_ pushData b
      mapM_ popDataExpecting c
      mapM_ pushData d
    pushData result

  Builtin.Div -> intsToInt

  Builtin.Down -> do
    x <- freshName
    a <- popDataExpecting $ VecType (ScalarVar x)
    pushData a

  Builtin.Drop -> popData_

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.Eq -> intsToBool

  Builtin.Empty -> do
    x <- freshName
    popDataExpecting_ $ VecType (ScalarVar x)
    pushData BoolType

  Builtin.Fun -> do
    a <- popData
    pushData $ Composition [] :> Composition [a]

  Builtin.Ge -> intsToBool

  Builtin.Gt -> intsToBool

  Builtin.If -> do
    popDataExpecting_ BoolType
    w <- freshName
    x <- freshName
    y <- freshName
    z <- freshName
    b <- popDataExpecting (RowVar w :> RowVar x)
    -- FIXME Unsafe.
    a@(_ :> Composition production) <- popDataExpecting
      $ RowVar y :> RowVar z
    if a == b
      then mapM_ pushData production
      else typeError "Mismatched types in 'if' branches"

  Builtin.Le -> intsToBool

  Builtin.Length -> do
    x <- freshName
    popDataExpecting_ $ VecType (ScalarVar x)
    pushData IntType

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

  Builtin.Top -> do
    x <- freshName
    VecType a <- popDataExpecting $ VecType (ScalarVar x)
    pushData a

  Builtin.Up -> do
    x <- freshName
    a <- popDataExpecting $ VecType (ScalarVar x)
    pushData a

  Builtin.Vec -> do
    a <- popData
    pushData $ VecType a

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

typeError :: String -> TypecheckM a
typeError message = do
  location <- here
  lift . Left $ TypeError location message

internalError :: String -> TypecheckM a
internalError = lift . Left . InternalError

popData_ :: Typecheck
popData_ = void popData

popData :: TypecheckM (Type Scalar)
popData = do
  hypothetical <- gets envHypothetical
  if hypothetical
    then ScalarVar <$> freshName
    else do
      dataStack <- gets envData
      case dataStack of
        [] -> typeError
          "expecting value but got empty stack"
        (top : down) -> do
          modify $ \ env -> env { envData = down }
          return top

popDataExpecting_ :: Type Scalar -> Typecheck
popDataExpecting_ = void . popDataExpecting

popDataExpecting :: Type Scalar -> TypecheckM (Type Scalar)
popDataExpecting type_ = do
  hypothetical <- gets envHypothetical
  if hypothetical
    then do
      var <- ScalarVar <$> freshName
      unify type_ var
      return var
    else do
      dataStack <- gets envData
      case dataStack of
        [] -> typeError $ unwords
          ["expecting", show type_, "but got empty stack"]
        (top : down) -> do
          modify $ \ env -> env { envData = down }
          unify type_ top
          return top

pushData :: Type Scalar -> Typecheck
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

getLocal :: Name -> TypecheckM (Type Scalar)
getLocal (Name index) = gets ((!! index) . envLocals)

pushLocal :: Type Scalar -> Typecheck
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }

hypothetically :: TypecheckM a -> TypecheckM (Env, Env)
hypothetically action = do
  before <- get
  modify $ \ env@Env{..} -> env { envHypothetical = True }
  void action
  modify $ \ env@Env{..} -> env { envHypothetical = False }
  after <- get
  put before
  return (before, after)

unify :: Type Scalar -> Type Scalar -> Typecheck
unify a b = do
  env <- get
  unify' (substChain env a) (substChain env b)

unify'
  :: Type Scalar
  -> Type Scalar
  -> Typecheck
unify' type1 type2 = case (type1, type2) of
  (a, b) | a == b -> return ()
  (ScalarVar a, _) -> unifyScalarVar a type2
  (_, ScalarVar b) -> unifyScalarVar b type1
  (_, Composition [] :> Composition [b]) -> unify' type1 b
  (Composition [] :> Composition [a], _) -> unify' a type2
  (a :> b, c :> d) -> unifyRow a c >> unifyRow b d
  (VecType a, VecType b) -> unify' a b
  (TupleType as, TupleType bs) -> unifyEach as bs
  _ -> typeError $ unwords
    [ "unable to unify"
    , show type1
    , "with"
    , show type2
    ]

-- TODO Occurs check.
unifyScalarVar :: Name -> Type Scalar -> Typecheck
unifyScalarVar name type_ = do
  mExisting <- gets (lookup name . envScalarVars)
  case mExisting of
    Just existing -> unless (existing == type_)
      . typeError $ unwords
        [ "unable to unify"
        , show type_
        , "with"
        , show existing
        ]
    Nothing -> declareScalar name type_

lookupScalar
  :: Env
  -> Name
  -> Either CompileError (Type Scalar)
lookupScalar Env{..} name
  = case lookup name envScalarVars of
    Nothing -> Left . InternalError $ unwords
      [ "nonexistent type variable "
      , show $ ScalarVar name
      , "!"
      ]
    Just existing -> Right existing

lookupRow
  :: Env
  -> Name
  -> Either CompileError (Type Row)
lookupRow Env{..} name
  = case lookup name envRowVars of
    Nothing -> Left . InternalError $ unwords
      [ "nonexistent type variable "
      , show $ RowVar name
      , "!"
      ]
    Just existing -> Right existing

-- TODO Occurs check.
unifyRowVar :: Name -> Type Row -> Typecheck
unifyRowVar name1 (RowVar name2)
  | name1 == name2 = return ()
unifyRowVar name type_ = do
  mExisting <- gets (lookup name . envRowVars)
  case mExisting of
    Just existing -> unless (existing == type_)
      . typeError $ unwords
        [ "unable to unify"
        , show $ RowVar name
        , "with"
        , show type_
        ]
    Nothing -> declareRow name type_

unifyRow
  :: Type Row
  -> Type Row
  -> Typecheck
unifyRow type1 type2 = case (type1, type2) of
  (a, b) | a == b -> return ()
  (RowVar a, _) -> unifyRowVar a type2
  (_, RowVar b) -> unifyRowVar b type1
  (Composition as, Composition bs) -> unifyEach as bs
  _ -> typeError $ unwords
    [ "unable to unify"
    , show type1
    , "with"
    , show type2
    ]

unifyEach
  :: [Type Scalar]
  -> [Type Scalar]
  -> Typecheck
unifyEach as bs
  | length as == length bs
  = mapM_ (uncurry unify') (zip as bs)
unifyEach as bs
  = typeError $ unwords
    [ "unable to unify"
    , show $ Composition as
    , "with"
    , show $ Composition bs
    ]

declareScalar :: Name -> Type Scalar -> Typecheck
declareScalar name type_ = modify $ \ env@Env{..}
  -> env { envScalarVars = (name, type_) : envScalarVars }

declareRow :: Name -> Type Row -> Typecheck
declareRow name type_ = modify $ \ env@Env{..}
  -> env { envRowVars = (name, type_) : envRowVars }

here :: TypecheckM Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

withLocation :: Location -> TypecheckM a -> TypecheckM a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
