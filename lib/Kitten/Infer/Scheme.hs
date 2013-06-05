{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Scheme
  ( free
  , generalize
  , instantiate
  , instantiateM
  -- , makeInstantiation
  , occurs
  , subDef
  , subFragment
  , sub
  , subChain
  , subTerm
  , subValue
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Set (Set)  -- (\\)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kitten.Def
import Kitten.Fragment
import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type
import Kitten.Typed
import Kitten.Util.Monoid

instantiateM :: Scheme -> Inferred Type
instantiateM = Inferred . lift . state . instantiate

instantiate :: Scheme -> Env -> (Type, Env)
instantiate (Forall names type_) env = let
  (renamedEnv, env') = foldr rename (emptyEnv, env) (Set.toList names)
  in (sub renamedEnv type_, env')
  where
  rename :: Name -> (Env, Env) -> (Env, Env)
  rename name (localEnv, globalEnv)
    = let (var, globalEnv') = freshVar globalEnv
    in (declare name var localEnv, globalEnv')

generalize :: Inferred Type -> Inferred Scheme
generalize action = do
  before <- getEnv
  type_ <- action
  after <- getEnv
  let
    substituted = sub after type_
    dependent = dependentBetween before after
    names = Set.filter dependent $ free substituted
  {- . regeneralize after -}
  return $ Forall names substituted

{-
regeneralize :: Env -> Scheme -> Scheme
regeneralize env (Forall rows scalars wholeType) = let
  (type_, vars) = runWriter $ regeneralize' True wholeType
  in Forall (foldr Set.delete rows vars) scalars type_

  where
  regeneralize' :: Bool -> Type a -> Writer [Name] (Type a)
  regeneralize' topLevel type_ = case type_ of
    (a :> b)
      | not topLevel
      , TypeVar c <- bottommost a
      , TypeVar d <- bottommost b
      , c == d
      , occurrencesScalar c wholeType env == 2
      -> do
        tell [c]
        trace ("Regeneralized var " ++ show c) (return ())
        return . Polytype $ row c type_
    (a :> b) -> (:>) <$> regeneralize' False a <*> regeneralize' False b
    (a :. b) -> (:.) <$> regeneralize' False a <*> regeneralize' False b
    _ -> return type_

bottommost :: Type Row -> Type Row
bottommost type_ = case type_ of
  (a :. _) -> bottommost a
  _ -> type_
-}

free :: Type -> Set Name
free type_ = case type_ of
  a :* b -> free a <> free b
  a :> b -> mconcatMap free a <> mconcatMap free b
  BoolType -> mempty
  CharType -> mempty
  FloatType -> mempty
  GeneratedType -> mempty
  HandleType -> mempty
  IntType -> mempty
  TestType -> mempty
  TypeVar name -> Set.singleton name
  UnitType -> mempty
  VectorType a -> free a

{-
freeScheme :: Scheme -> (Set Name, Set Name)
freeScheme (Forall rows scalars type_)
  = let (rows', scalars') = free type_
  in (rows' \\ rows, scalars' \\ scalars)
-}

{-
makeInstantiation :: Name -> Inferred (Type Scalar)
makeInstantiation name = do
  here <- getsEnv envLocation
  type_ <- freshFunctionM
  modifyEnv $ \ env -> env
    { envInstantiations = Instantiation
      { instantiationLocation = here
      , instantiationName = name
      , instantiationType = type_
      } : envInstantiations env
    }
  return type_
-}

occurs :: Name -> Env -> Type -> Bool
occurs = (((> 0) .) .) . occurrences

occurrences :: Name -> Env -> Type -> Int
occurrences name env type_ = case type_ of
  a :* b -> occurrences name env a + occurrences name env b
  a :> b -> sum (map (occurrences name env) a) + sum (map (occurrences name env) b)
  BoolType -> 0
  CharType -> 0
  FloatType -> 0
  GeneratedType -> 0
  HandleType -> 0
  IntType -> 0
  TestType -> 0
  TypeVar name' -> case findType env name' of
    Left{} -> if name == name' then 1 else 0
    Right type' -> occurrences name env type'
  UnitType -> 0
  VectorType a -> occurrences name env a

{-
occursScheme :: Name -> Scheme -> Env -> Bool
occursScheme name (Forall rows scalars type_) env
  = not (name `Set.member` rows || name `Set.member` scalars)
  && occurs name type_ env
-}

-- | Tests whether a variable is dependent between two type
-- environment states.
dependentBetween :: Env -> Env -> Name -> Bool
dependentBetween before after name
  = any (bound after) (unbound before)
  where
  bound env name' = occurs name env (TypeVar name')

-- | Enumerates those type variables in an environment which
-- are allocated but not yet bound to a type.
unbound :: Env -> [Name]
unbound Env{..} = filter
  (not . (`Map.member` envTypes))
  [Name 0 .. pred envNext]

subChain :: Env -> Type -> Type
subChain env type_ = case type_ of
  TypeVar name
    | Right type' <- findType env name
    -> subChain env type'
  _ -> type_

subDef :: Env -> Def Value -> Def Value
subDef env def@Def{..} = def
  { defName = defName
  , defTerm = subValue env defTerm
  }

subFragment
  :: Fragment Value Typed -> Env -> Fragment Value Typed
subFragment Fragment{..} env = Fragment
  { fragmentDefs = map (subDef env) fragmentDefs
  , fragmentTerms = map (subTerm env) fragmentTerms
  }

{-
subScheme :: Env -> Scheme -> Scheme
subScheme env (Forall rows scalars type_)
  = Forall rows scalars (subScalar env' type_)
  where
  env' :: Env
  env' = env
    { envRows = foldr Map.delete (envRows env) (Set.toList rows)
    , envScalars = foldr Map.delete (envScalars env) (Set.toList scalars)
    }
-}

subTerm :: Env -> Typed -> Typed
subTerm env typed = case typed of
  Call name loc -> Call name loc
  Compose terms -> Compose (map (subTerm env) terms)
  Builtin builtin loc
    -> Builtin builtin loc
  If true false loc
    -> If true false loc
  Push value loc
    -> Push (subValue env value) loc
  Scoped term loc
    -> Scoped (subTerm env term) loc

sub :: Env -> Type -> Type
sub env type_ = case type_ of
  a :> b -> map (sub env) a :> map (sub env) b
  a :* b -> sub env a :* sub env b
  BoolType{} -> type_
  CharType{} -> type_
  FloatType{} -> type_
  GeneratedType{} -> type_
  IntType{} -> type_
  HandleType{} -> type_
  TestType -> type_
  TypeVar name
    | Right type' <- findType env name
    -> sub env type'
    | otherwise
    -> type_
  UnitType{} -> type_
  VectorType a -> VectorType (sub env a)

subValue :: Env -> Value -> Value
subValue env value = case value of
  Activation values term -> Activation
    (map (subValue env) values)
    (subTerm env term)
  Bool{} -> value
  Char{} -> value
  Closed name -> Closed name
  Closure anno names term
    -> Closure anno names (subTerm env term)
  Escape names -> Escape names
  Float{} -> value
  Function anno term -> Function anno (subTerm env term)
  Handle{} -> value
  Int{} -> value
  Local name -> Local name
  Pair first second
    -> Pair (subValue env first) (subValue env second)
  Unit{} -> value
  Vector anno values -> Vector anno (map (subValue env) values)
