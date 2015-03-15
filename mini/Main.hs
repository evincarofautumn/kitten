{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import System.IO.Unsafe
import Test.HUnit
import Test.Hspec
import Text.Parsec hiding (many, parse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zonking" $ do
    it "does nothing to free type variables"
      $ assertEqual "" (zonkType emptyTEnv va) va
    it "substitutes one level"
      $ assertEqual "" (zonkType emptyTEnv { envTvs = Map.singleton ia vb } va) vb
    it "substitutes multiple levels"
      $ assertEqual ""
        (zonkType emptyTEnv { envTvs = Map.fromList [(ia, vb), (ib, cint)] } va)
        cint
  describe "type inference" $ do
    it "gives the identity type for the empty program"
      $ testScheme (inferEmpty (parse ""))
      $ TForall ir kr (TForall ie ke ((vr .-> vr) ve))
    it "gives the composed type from simple composition"
      $ testScheme (inferEmpty (parse "1 2 add"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr .* TCon CInt) ve))
    it "gives the composed type for higher-order composition"
      $ testScheme (inferEmpty (parse "1 quo 2 quo com [add] com app"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr .* TCon CInt) ve))
    it "deduces simple side effects"
      $ testScheme (inferEmpty (parse "1 say"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr) (cio .| ve)))
    it "deduces higher-order side effects"
      $ testScheme (inferEmpty (parse "1 [say] app"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr) (cio .| ve)))
    it "fails on basic type mismatches"
      $ testFail (inferEmpty (parse "1 [add] add"))
    it "correctly copies from a local"
      $ testScheme (inferEmpty (parse "1 &x +x"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr .* cint) ve))
    it "correctly copies multiple times from a local"
      $ testScheme (inferEmpty (parse "1 &x +x +x"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr .* cint .* cint) ve))
    it "correctly moves from a local"
      $ testScheme (inferEmpty (parse "1 &x -x"))
      $ TForall ir kr (TForall ie ke ((vr .-> vr .* cint) ve))
    it "fails when moving from a moved-from local"
      $ testFail (inferEmpty (parse "1 &x -x -x"))
    it "fails when copying from a moved-from local"
      $ testFail (inferEmpty (parse "1 &x -x +x"))
  describe "definitions" $ do
    it "checks the type of a definition" $ let
      idType = TForall ir kr (TForall ia kv (TForall ie ke ((vr .* va .-> vr .* va) ve)))
      in testScheme
        ((\ (_, _, t) -> t) <$> inferTypes
          (Map.fromList [("id", (idType, parse "&x -x"))])
          (parse "id"))
        idType
  where
  inferEmpty expr = do
    (_, scheme, _) <- inferType0 Map.empty expr
    return scheme
  testFail action = do
    result <- runTc action
    assert $ either (const True) (const False) result
  testScheme inference expected = do
    result <- runTc $ do
      scheme <- inference
      instanceCheck scheme expected
    either assertFailure (const (return ())) result

  ir = TypeId 0
  vr = TVar kr ir
  ia = TypeId 1
  va = TVar kv ia
  ib = TypeId 2
  vb = TVar kv ib
  ie = TypeId 3
  ve = TVar ke ie
  kr = Just (KVar (KindId 0))
  kv = Just (KVar (KindId 1))
  ke = Just (KVar (KindId 2))

data Expr

-- The identity function.

  = EId TRef

-- Compose two expressions.

  | ECat TRef Expr Expr

-- Quote an expression.

  | EQuote TRef Expr

-- Invoke a word, possibly with some generic type parameters.

  | ECall TRef Name [Type]

-- Push a value to the stack.

  | EPush TRef Val

-- Send a value to the locals from the stack.

  | EGo TRef Name

-- Bring a value from the locals to the stack.

  | ECome HowCome TRef Name

  deriving (Eq)

-- A nullable reference to a type.

type TRef = Maybe Type

data Type

-- Type constructor.

  = TCon !Con

-- Type variable, used during unification.

  | TVar !KRef !TypeId

-- Skolem constant, used during instance checking.

  | TConst !TypeId

-- Quantified type, used for both prenex and higher-rank polymorphism.
-- Quantifiers always quantify over value-kinded types; the type reference here
-- indicates the kind of the quantified type variable.

  | TForall !TypeId !KRef !Type

-- Application of type constructor to type.

  | TApp !Type !Type

  deriving (Eq)

-- A nullable reference to a kind.

type KRef = Maybe Kind

-- Type variables are distinguished by a "type identifier", which is generated
-- afresh from the typing environment.

newtype TypeId = TypeId { unTypeId :: Int }
  deriving (Enum, Eq, Ord)

data Con

-- Integers.

  = CInt

-- Constructor for functions.

  | CFun

-- Constructor for stacks.

  | CProd

-- Constructor for vectors.

  | CVec

-- The pure effect.

  | CPure

-- The io effect.

  | CIO

-- The fail effect.

  | CFail

-- Constructor for effect rows.

  | CJoin

  deriving (Eq)

-- Common constructors.

cint, cfun, cprod, cvec, cpure, cio, cfail, cjoin :: Type
cint = TCon CInt
cfun = TCon CFun
cprod = TCon CProd
cvec = TCon CVec
cpure = TCon CPure
cio = TCon CIO
cfail = TCon CFail
cjoin = TCon CJoin

-- A kind is the type of a type. Types with the "value" kind are inhabited by
-- values; all other types are used only to enforce program invariants, such as
-- the restriction that the stack may only contain values, or that a function
-- only has a particular effect.

data Kind

-- Kind of values.

  = KVal

-- Kind of stacks.

  | KRho

-- Kind of effect labels.

  | KEff

-- Kind of effect rows.

  | KEffRho

-- Kind variable.

  | KVar !KindId

-- Kind of type constructors.

  | KFun !Kind !Kind

  deriving (Eq)

-- Kind variables are distinguished by "kind identifiers", which are produced in
-- a separate namespace from type identifiers.

newtype KindId = KindId { unKindId :: Int }
  deriving (Enum, Eq, Ord)

-- A value is an instance of a type, which may be placed on the stack.

data Val = VInt Int deriving (Eq)

-- Definitions are indexed by their names.

type Name = Text

-- The typing environment tracks the state of inference.

data TEnv = TEnv {

-- type variable -> type

  envTvs :: Map TypeId Type,

-- type variable -> kind

  envTks :: Map TypeId Kind,

-- kind variable -> kind

  envKvs :: Map KindId Kind,

-- local variable -> type

  envVs :: Map Name Type,

-- word -> signature

  envSigs :: Map Name Type,

-- The current state of globally unique type and kind ID generation.

  envCurrentType :: IORef TypeId,
  envCurrentKind :: IORef KindId }

-- An empty typing environment.

emptyTEnv :: TEnv
emptyTEnv = TEnv {
  envTvs = Map.empty,
  envTks = Map.empty,
  envKvs = Map.empty,
  envVs = Map.empty,
  envSigs = Map.empty,
  envCurrentType = currentTypeId,
  envCurrentKind = currentKindId }

-- The current state of globally unique type and kind ID generation.

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

currentKindId :: IORef KindId
currentKindId = unsafePerformIO (newIORef (KindId 0))
{-# NOINLINE currentKindId #-}

-- To infer the types in a mutually-recursive binding group, the type of each
-- definition is inferred under the assumption that all signatures in the
-- binding group are correct, then the inferred type is checked against the
-- declared type.

inferTypes :: Map Name (Type, Expr) -> Expr -> Tc (Map Name (Type, Expr), Expr, Type)
inferTypes defs expr0 = do
  inferredDefs <- fmap Map.fromList . mapM go . Map.toList $ defs
  (expr1, scheme, _kind) <- inferType0 sigs expr0
  return (inferredDefs, expr1, scheme)
  where
  sigs = Map.map fst defs
  go (name, (scheme, expr)) = do
    (expr', scheme', _) <- inferType0 sigs expr
    instanceCheck scheme' scheme
    return (name, (scheme, expr'))

-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0 :: Map Name Type -> Expr -> Tc (Expr, Type, Kind)
inferType0 sigs expr = while ["inferring the type of", show expr] $ do
  rec (expr', t, kind, tenvFinal) <- inferType tenvFinal emptyTEnv { envSigs = sigs } expr
  expr'' <- defaultExprKinds tenvFinal expr'
  regeneralized <- regeneralize tenvFinal <$> defaultTypeKinds tenvFinal (zonkType tenvFinal t)
  return (zonkExpr tenvFinal expr'', regeneralized, kind)

-- The default kind of a type is the value kind.

defaultExprKinds :: TEnv -> Expr -> Tc Expr
defaultExprKinds tenv e = case e of
  EId (Just t) -> EId <$> (Just <$> defaultTypeKinds tenv t)
  ECat (Just t) e1 e2 -> ECat <$> (Just <$> defaultTypeKinds tenv t) <*> defaultExprKinds tenv e1 <*> defaultExprKinds tenv e2
  EQuote (Just t) e' -> EQuote <$> (Just <$> defaultTypeKinds tenv t) <*> defaultExprKinds tenv e'
  ECall (Just t) name params -> do
    params' <- mapM (defaultTypeKinds tenv) params
    paramKinds <- mapM (fmap (\ (_, k, _) -> k) . inferKind tenv) params'
    let valueParams = map fst . filter ((== KVal) . snd) $ zip params' paramKinds
    ECall <$> (Just <$> defaultTypeKinds tenv t) <*> pure name <*> pure valueParams
  EPush (Just t) val -> EPush <$> (Just <$> defaultTypeKinds tenv t) <*> defaultValKinds tenv val
  EGo (Just t) name -> EGo <$> (Just <$> defaultTypeKinds tenv t) <*> pure name
  ECome how (Just t) name -> ECome how <$> (Just <$> defaultTypeKinds tenv t) <*> pure name
  _ -> unannotated "expression" e

defaultValKinds :: TEnv -> Val -> Tc Val
defaultValKinds _tenv v@VInt{} = return v

defaultTypeKinds :: TEnv -> Type -> Tc Type
defaultTypeKinds tenv t = case t of
  TCon{} -> return t
  TVar (Just k) x -> TVar <$> (Just <$> defaultKindKinds tenv k) <*> pure x
  -- FIXME: This isn't supposed to happen.
  TVar Nothing x -> return $ TVar (Just KVal) x
  TConst{} -> return t
  TForall tv (Just k) t' -> TForall tv <$> (Just <$> defaultKindKinds tenv k) <*> pure t'
  TForall _ Nothing _ -> unannotated "type" t
  TApp t1 t2 -> TApp <$> defaultTypeKinds tenv t1 <*> defaultTypeKinds tenv t2

defaultKindKinds :: TEnv -> Kind -> Tc Kind
defaultKindKinds tenv k = let k' = zonkKind tenv k in case k' of
  KVal -> return k'
  KRho -> return k'
  KEff -> return k'
  KEffRho -> return k'
  KVar x -> case Map.lookup x (envKvs tenv) of
    Just{} -> return k'
    Nothing -> return KVal
  KFun a b -> KFun <$> defaultKindKinds tenv a <*> defaultKindKinds tenv b

unannotated :: (Show a) => String -> a -> Tc b
unannotated thing x = fail $ unwords ["cannot default kind of un-annotated", thing, show x]

-- Kind inference lets us distinguish types from type constructors, and value
-- types from effect types.

inferKind :: TEnv -> Type -> Tc (Type, Kind, TEnv)
inferKind tenv0 t = while ["inferring the kind of", show t] $ case t of
  TCon con -> return (TCon con, case con of
    CInt -> KVal
    CFun -> KRho ..-> KRho ..-> KEffRho ..-> KVal
    CProd -> KRho ..-> KVal ..-> KRho
    CVec -> KVal ..-> KVal
    CPure -> KEff
    CIO -> KEff
    CFail -> KEff
    CJoin -> KEff ..-> KEffRho ..-> KEffRho, tenv0)
  TVar (Just k) _ -> return (t, k, tenv0)
  TVar Nothing x -> case Map.lookup x (envTks tenv0) of
    Just k' -> return (TVar (Just k') x, k', tenv0)
    Nothing -> do
      k' <- freshKv tenv0
      return (TVar (Just k') x, k', tenv0 { envTks = Map.insert x k' (envTks tenv0) })
  TConst{} -> fail "cannot infer kind of skolem constant"
  TForall tv ma t' -> do
    a <- maybe (freshKv tenv0) return ma
    let tenv' = tenv0 { envTks = Map.insert tv a (envTks tenv0) }
    (t'', k1, _) <- inferKind tenv' t'
    -- Quantifiers should only wrap value-kinded types (usually functions).
    _ <- unifyKind tenv' k1 KVal
    return (TForall tv (Just a) t'', k1, tenv0)
  TApp t1 t2 -> do
    (t1', k1, tenv1) <- inferKind tenv0 t1
    (t2', k2, tenv2) <- inferKind tenv1 t2
    ka <- freshKv tenv2
    kb <- freshKv tenv2
    tenv3 <- unifyKind tenv2 k1 (ka ..-> kb)
    tenv4 <- unifyKind tenv3 k2 ka
    return (t1' .$ t2', kb, tenv4)

-- Kind unification proceeds similarly to type unification.

unifyKind :: TEnv -> Kind -> Kind -> Tc TEnv
unifyKind tenv0 k1 k2 = case (k1, k2) of
  _ | k1 == k2 -> return tenv0
  (KVar x, t) -> unifyKv tenv0 x t
  (_, KVar{}) -> commute
  (a `KFun` b, c `KFun` d) -> do
    tenv1 <- unifyKind tenv0 a c
    unifyKind tenv1 b d
  _ -> fail $ unwords ["cannot unify kinds", show k1, "and", show k2]
  where
  commute = unifyKind tenv0 k2 k1

-- Inferring the type of an expression is straightforward. Most of these cases
-- simply encode the type signatures of various intrinsic functions.

inferType :: TEnv -> TEnv -> Expr -> Tc (Expr, Type, Kind, TEnv)
inferType tenvFinal tenv0 expr0 = while ["inferring the type of", show expr0] $ case expr0 of

-- Pushing a value results in a stack with that value on top.

  EPush Nothing val -> do
    let (val', t, tenv1) = inferVal tenv0 val
    a <- freshTv tenv1
    e <- freshTv tenv1
    (type_, k, tenv2) <- inferKind tenv1 $ (a .-> a .* t) e
    let type' = zonkType tenvFinal type_
    return (EPush (Just type') val', type_, k, tenv2)

-- Pure intrinsics.

  ECall Nothing name@"add" [] -> do
    a <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* cint .* cint .-> a .* cint) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)
  ECall Nothing name@"com" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    c <- freshTv tenv0
    d <- freshTv tenv0
    e1 <- freshTv tenv0
    e2 <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* (b .-> c) e1 .* (c .-> d) e1 .-> a .* (b .-> d) e1) e2
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)
  ECall Nothing name@"app" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* (a .-> b) e .-> b) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)
  ECall Nothing name@"quo" [] -> do
    a  <- freshTv tenv0
    b  <- freshTv tenv0
    c  <- freshTv tenv0
    e1 <- freshTv tenv0
    e2 <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* b .-> a .* (c .-> c .* b) e1) e2
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

-- Vector intrinsics.

  ECall Nothing name@"vec" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* b .-> a .* (cvec .$ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

  ECall Nothing name@"head" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* (cvec .$ b) .-> a .* b) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

  ECall Nothing name@"tail" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* (cvec .$ b) .-> a .* (cvec .$ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

  ECall Nothing name@"cat" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* (cvec .$ b) .* (cvec .$ b) .-> a .* (cvec .$ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

-- Effectful intrinsics. Note that we allow the effect to be polymorphic in its
-- tail, so that effects can be composed.

  ECall Nothing name@"say" [] -> do
    a <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .* cint .-> a) (cio .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

  ECall Nothing name@"abort" [] -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    (type_, k, tenv1) <- inferKind tenv0 $ (a .-> b) (cfail .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, k, tenv1)

-- The type of a definition is simply looked up in the environment.

  ECall Nothing name [] -> case Map.lookup name (envSigs tenv0) of
    Just t@TForall{} -> do
      (type_, params, tenv1) <- instantiatePrenex tenv0 t
      let type' = zonkType tenvFinal type_
      return (ECall (Just type') name params, type_, KVal, tenv1)
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> fail $ "cannot infer type of " ++ show name

-- The type of the composition of two expressions is the composition of the
-- types of those expressions.

  ECat Nothing expr1 expr2 -> do
    (expr1', t1, _, tenv1) <- inferType' tenv0 expr1
    (expr2', t2, _, tenv2) <- inferType' tenv1 expr2
    (a, b, e1, tenv3) <- unifyFun tenv2 t1
    (c, d, e2, tenv4) <- unifyFun tenv3 t2
    tenv5 <- unifyType tenv4 b c
    tenv6 <- unifyType tenv5 e1 e2
    (type_, k, tenv7) <- inferKind tenv6 $ (a .-> d) e1
    let type' = zonkType tenvFinal type_
    return (ECat (Just type') expr1' expr2', type_, k, tenv7)

-- The empty program is the identity function on stacks.

  EId Nothing -> do
    a <- freshTv tenv0
    e <- freshTv tenv0
    let type_ = (a .-> a) e
    let type' = zonkType tenvFinal type_
    return (EId (Just type'), type_, KVal, tenv0)

-- A quoted expression is pushed rather than being evaluated.

  EQuote Nothing expr -> do
    a <- freshTv tenv0
    e <- freshTv tenv0
    (expr', b, _, tenv1) <- inferType' tenv0 expr
    let type_ = (a .-> a .* b) e
    let type' = zonkType tenvFinal type_
    return (EQuote (Just type') expr', type_, KVal, tenv1)

-- A value going from the stack to the locals.

  EGo Nothing name -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    let type_ = (a .* b .-> a) e
    let type' = zonkType tenvFinal type_
    return (EGo (Just type') name, type_, KVal, tenv0 { envVs = Map.insert name b (envVs tenv0) })

-- A value coming from the locals to the stack. If it's coming by moving, its
-- name is removed from the local scope. This relies on composition being
-- inferred in left-to-right order.

  ECome how Nothing name -> do
    a <- freshTv tenv0
    e <- freshTv tenv0
    b <- case Map.lookup name (envVs tenv0) of
      Just t -> return t
      Nothing -> fail $ "unbound variable " ++ Text.unpack name
    let
      tenv1 = case how of
        Move -> tenv0 { envVs = Map.delete name (envVs tenv0) }
        Copy -> tenv0
    let type_ = (a .-> a .* b) e
    let type' = zonkType tenvFinal type_
    return (ECome how (Just type') name, type_, KVal, tenv1)

  _ -> fail $ unwords ["cannot infer type of already-annotated expression ", show expr0]

  where
  inferType' = inferType tenvFinal

-- A convenience function for unifying a type with a function type.

unifyFun :: TEnv -> Type -> Tc (Type, Type, Type, TEnv)
unifyFun tenv0 t = case t of
  TCon CFun `TApp` a `TApp` b `TApp` e -> return (a, b, e, tenv0)
  _ -> do
    a <- freshTv tenv0
    b <- freshTv tenv0
    e <- freshTv tenv0
    tenv1 <- unifyType tenv0 t ((a .-> b) e)
    return (a, b, e, tenv1)

-- Type unification. There are two kinds of unification going on here: basic
-- logical unification for value types, and row unification for effect types.

unifyType :: TEnv -> Type -> Type -> Tc TEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> return tenv0
  (TVar _ x, t) -> unifyTv tenv0 x t
  (_, TVar{}) -> commute
  -- FIXME: Unify the kinds here?
  (a, TForall x mk t) -> do
    k <- maybe (freshKv tenv0) return mk
    (b, _, tenv1) <- instantiate tenv0 x k t
    unifyType tenv1 a b
  (TForall{}, _) -> commute

-- Row unification is essentially unification of sets: we attempt to rewrite one
-- row into the other by swapping and dropping, and if we can do so then we know
-- they are isomorphic and unify successfully.
--
-- The occurs check prevents us from unifying rows with a common tail and a
-- distinct prefix, which could fail to terminate because the algorithm
-- generates fresh type variables.

  (TCon CJoin `TApp` l `TApp` r, s) -> do
    ms <- rowIso tenv0 l s
    case ms of
      Just (TCon CJoin `TApp` _ `TApp` s', substitution, tenv1) -> case substitution of
        Just (x, t)
          | occurs tenv0 x (effectTail r)
          -> fail $ unwords ["cannot unify effects", show t1, "and", show t2]
          | otherwise -> let
            tenv2 = tenv1 { envTvs = Map.insert x t (envTvs tenv1) }
            in unifyType tenv2 r s'
        Nothing -> unifyType tenv1 r s'

      -- HACK: Duplicates the remaining cases.
      _ -> case (t1, t2) of
        (a `TApp` b, c `TApp` d) -> do
          tenv1 <- unifyType tenv0 a c
          unifyType tenv1 b d
        _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

  (_, TCon CJoin `TApp` _ `TApp` _) -> commute

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and effect type constructors.

  (a `TApp` b, c `TApp` d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d

  _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  commute = unifyType tenv0 t2 t1

-- The row-isomorphism operation takes an effect label and an effect row, and
-- asserts that the row can be rewritten to begin with that label under some
-- substitution. It returns the substitution and the tail of the rewritten row.

rowIso :: TEnv -> Type -> Type -> Tc (Maybe (Type, Maybe (TypeId, Type), TEnv))

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 lin rin@(TCon CJoin `TApp` l `TApp` _)
  | l == lin = return $ Just (rin, Nothing, tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l (TCon CJoin `TApp` l' `TApp` r)
  | l /= l' = do
  ms <- rowIso tenv0 l r
  return $ case ms of
    Just (r', substitution, tenv1) -> Just (l .| l' .| r', substitution, tenv1)
    Nothing -> Nothing

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail.

rowIso tenv0 l (TVar _ a) = do
  b <- freshTv tenv0
  let res = l .| b
  return $ Just (res, Just (a, res), tenv0)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ = return Nothing

-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.

unifyTv :: TEnv -> TypeId -> Type -> Tc TEnv
unifyTv tenv0 x t = case t of
  TVar _ y | x == y -> return tenv0
  TVar{} -> declare
  _ -> if occurs tenv0 x t then fail "occurs check" else declare
  where
  declare = case Map.lookup x (envTvs tenv0) of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> return tenv0 { envTvs = Map.insert x t (envTvs tenv0) }

-- Kind variable unification is virtually identical to type variable
-- unification. This is currently missing an occurs check.

unifyKv :: TEnv -> KindId -> Kind -> Tc TEnv
unifyKv tenv0 x k = case k of
  KVar y | x == y -> return tenv0
  KVar{} -> declare
  _ -> if occursKind tenv0 x k then fail "occurs check" else declare
  where
  declare = case Map.lookup x (envKvs tenv0) of
    Just k2 -> unifyKind tenv0 k k2
    Nothing -> return tenv0 { envKvs = Map.insert x k (envKvs tenv0) }

-- We need to be able to count occurrences of a type variable in a type for two
-- reasons: to prevent infinite types (the "occurs check"), and to determine
-- whether a stack variable can be generalized to a higher rank.

occurrences :: TEnv -> TypeId -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TCon{} -> 0
    TVar _ y -> case Map.lookup y (envTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TConst{} -> 0
    TForall x' _ t'
      -> if x == x' then 0 else recur t'
    a `TApp` b -> recur a + recur b

occurs :: TEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0

occursKind :: TEnv -> KindId -> Kind -> Bool
occursKind tenv0 x = recur
  where
  recur k = case k of
    KVal -> False
    KRho -> False
    KEff -> False
    KEffRho -> False
    KVar y -> case Map.lookup y (envKvs tenv0) of
      Nothing -> x == y
      Just t' -> recur t'
    KFun a b -> recur a || recur b

-- The inferred type of a value is evident from its constructor.

inferVal :: TEnv -> Val -> (Val, Type, TEnv)
inferVal tenv val = case val of
  VInt{} -> (val, cint, tenv)

-- Zonking a type fully substitutes all type variables. That is, if you have:
--
--     t0 ~ t1
--     t1 ~ int
--
-- Then zonking "t0" gives you "int".

zonkType :: TEnv -> Type -> Type
zonkType tenv0 = recur
  where
  recur t = case t of
    TCon{} -> t
    TVar k x -> case Map.lookup x (envTvs tenv0) of
      Just (TVar _ x') | x == x' -> TVar (zonkKRef k) x
      Just t' -> recur t'
      Nothing -> t
    TConst{} -> t
    TForall x k t'
      -> TForall x (zonkKRef k) . zonkType tenv0 { envTvs = Map.delete x (envTvs tenv0) } $ t'
    a `TApp` b -> recur a .$ recur b
  zonkKRef = fmap (zonkKind tenv0)

-- Zonking a kind is similar to zonking a type.

zonkKind :: TEnv -> Kind -> Kind
zonkKind tenv0 = recur
  where
  recur k = case k of
    KVal -> k
    KRho -> k
    KEff -> k
    KEffRho -> k
    KVar x -> case Map.lookup x (envKvs tenv0) of
      Just (KVar x') | x == x' -> k
      Just k' -> recur k'
      Nothing -> k
    a `KFun` b -> recur a ..-> recur b

-- Zonking an expression zonks all the annotated types of the terms. This could
-- be done more efficiently by sharing type references and updating them
-- impurely, but this implementation is easier to get right and understand.

zonkExpr :: TEnv -> Expr -> Expr
zonkExpr tenv0 = recur
  where
  recur expr = case expr of
    EPush tref val -> EPush (zonkTRef tref) (zonkVal tenv0 val)
    ECall tref name params -> ECall (zonkTRef tref) name (map (zonkType tenv0) params)
    ECat tref e1 e2 -> ECat (zonkTRef tref) (recur e1) (recur e2)
    EQuote tref e -> EQuote (zonkTRef tref) (recur e)
    EId tref -> EId (zonkTRef tref)
    EGo tref name -> EGo (zonkTRef tref) name
    ECome how tref name -> ECome how (zonkTRef tref) name
  zonkTRef = fmap (zonkType tenv0)

-- Zonking a value might do something in the future, for more interesting kinds
-- of values.

zonkVal :: TEnv -> Val -> Val
zonkVal _tenv val@VInt{} = val

-- The free variables of a type are those not bound by any quantifier.

freeTvs :: Type -> Set TypeId
freeTvs = Set.fromList . Map.keys . freeTvks

freeTvks :: Type -> Map TypeId KRef
freeTvks t = case t of
  TCon{} -> Map.empty
  TVar k x -> Map.singleton x k
  TConst{} -> Map.empty
  TForall x _ t' -> Map.delete x (freeTvks t')
  a `TApp` b -> Map.union (freeTvks a) (freeTvks b)

-- Likewise for kinds, except kinds can't be higher-ranked anyway. I wonder what
-- that would even mean?

freeKvs :: Kind -> Set KindId
freeKvs k = case k of
  KVal -> Set.empty
  KRho -> Set.empty
  KEff -> Set.empty
  KEffRho -> Set.empty
  a `KFun` b -> freeKvs a `Set.union` freeKvs b
  KVar x -> Set.singleton x

-- Because all functions are polymorphic with respect to the part of the stack
-- they don't touch, all words of order n can be regeneralized to words of rank
-- n with respect to the stack-kinded type variables.
--
-- This means that if a stack-kinded type variable occurs only twice in a type,
-- in the bottommost position on both sides of a function arrow, then its scope
-- can be reduced to only that function arrow by introducing a higher-ranked
-- quantifier. For example, the type of "map":
--
--     map :: ∀ρσαβ. ρ × vector α × (σ × α → σ × β) → ρ × vector β
--
-- Can be regeneralized like so:
--
--     map :: ∀ραβ. ρ × vector α × (∀σ. σ × α → σ × β) → ρ × vector β
--
-- In order to correctly regeneralize a type, it needs to contain no
-- higher-ranked quantifiers.

regeneralize :: TEnv -> Type -> Type
regeneralize tenv t = let
  (t', vars) = runWriter $ go t
  in foldr (uncurry TForall) t' $ foldr (deleteBy ((==) `on` fst)) (Map.toList (freeTvks t')) vars
  where
  go :: Type -> Writer [(TypeId, KRef)] Type
  go t' = case t' of
    TCon CFun `TApp` a `TApp` b `TApp` e
      | TVar k c <- bottommost a
      , TVar _ d <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [(c, k)]
        a' <- go a
        b' <- go b
        e' <- go e
        return $ TForall c k ((a' .-> b') e')
    TCon CProd `TApp` a `TApp` b -> do
      a' <- go a
      b' <- go b
      return $ cprod .$ a' .$ b'
    TForall{} -> error "cannot regeneralize higher-ranked type"
    a `TApp` b -> TApp <$> go a <*> go b
    _ -> return t'

-- Utilities for operating on chains of things.

bottommost :: Type -> Type
bottommost (TCon CProd `TApp` a `TApp` _) = bottommost a
bottommost t = t

effectTail :: Type -> Type
effectTail (TCon CJoin `TApp` _ `TApp` a) = effectTail a
effectTail t = t

-- To instantiate a type scheme, we simply replace all quantified variables with
-- fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order.

instantiate :: TEnv -> TypeId -> Kind -> Type -> Tc (Type, Type, TEnv)
instantiate tenv0 x k t = do
  ia <- freshTypeId tenv0
  let a = TVar (Just k) ia
  replaced <- replaceTv tenv0 x a t
  return (replaced, a, tenv0 { envTks = Map.insert ia k (envTks tenv0) })

instantiatePrenex :: TEnv -> Type -> Tc (Type, [Type], TEnv)
instantiatePrenex tenv0 q@(TForall x mk t) = while ["instantiating", show q] $ do
  k <- maybe (freshKv tenv0) return mk
  (t', a, tenv1) <- instantiate tenv0 { envTks = Map.insert x k (envTks tenv0) } x k t
  (t'', as, tenv2) <- instantiatePrenex tenv1 t'
  return (t'', a : as, tenv2)
instantiatePrenex tenv0 t = return (t, [], tenv0)

-- Capture-avoiding substitution of a type variable with a
-- type throughout a type.

replaceTv :: TEnv -> TypeId -> Type -> Type -> Tc Type
replaceTv tenv0 x a = recur
  where
  recur t = case t of
    TForall x' k t'
      | x == x' -> return t
      | x' `Set.notMember` freeTvs t' -> TForall x' k <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- replaceTv tenv0 x' (TVar k z) t'
        TForall z k <$> recur t''
    TVar _ x' | x == x' -> return a
    m `TApp` n -> TApp <$> recur m <*> recur n
    _ -> return t

-- Skolemization replaces quantified type variables with type constants.

skolemize :: TEnv -> Type -> Tc (Set TypeId, Type)
skolemize tenv0 t = case t of
  TForall x _ t' -> do
    k <- freshTypeId tenv0
    let tenv1 = tenv0 { envTvs = Map.insert x (TConst k) (envTvs tenv0) }
    (k', t'') <- skolemize tenv1 (zonkType tenv1 t')
    return (Set.insert k k', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  TCon CFun `TApp` a `TApp` b `TApp` e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, (a .-> b') e)
  _ -> return (Set.empty, t)

-- Since skolem constants only unify with type variables and themselves,
-- unifying a skolemized scheme with a type tells you whether one is a generic
-- instance of the other. This is used to check the signatures of definitions.

instanceCheck :: Type -> Type -> Tc ()
instanceCheck inferredScheme declaredScheme = do
  let tenv0 = emptyTEnv
  (inferredType, _, tenv1) <- instantiatePrenex tenv0 inferredScheme
  (ids, declaredType) <- skolemize tenv1 declaredScheme
  check <- Tc $ do
    result <- runTc $ subsumptionCheck tenv1 inferredType declaredType
    return $ Right result
  _tenv2 <- case check of
    Left message -> fail (prefix ++ " (instantiated to " ++ show inferredType ++ " and " ++ show declaredType ++ ")" ++ ": " ++ message)
    Right{} -> return tenv1
  let escaped = freeTvs inferredScheme `Set.union` freeTvs declaredScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) (fail prefix)
  return ()
  where
  prefix = unwords [show inferredScheme, "is not an instance of", show declaredScheme]

subsumptionCheck :: TEnv -> Type -> Type -> Tc TEnv

subsumptionCheck tenv0 (TForall x mk t) t2 = do
  k <- maybe (freshKv tenv0) return mk
  (t1, _, tenv1) <- instantiate tenv0 x k t
  subsumptionCheck tenv1 t1 t2

subsumptionCheck tenv0 t1 (TCon CFun `TApp` a `TApp` b `TApp` e) = do
  (a', b', e', tenv1) <- unifyFun tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'

subsumptionCheck tenv0 (TCon CFun `TApp` a' `TApp` b' `TApp` e') t2 = do
  (a, b, e, tenv1) <- unifyFun tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'

subsumptionCheck tenv0 t1 t2 = unifyType tenv0 t1 t2

-- This handles contravariance.

subsumptionCheckFun :: TEnv -> Type -> Type -> Type -> Type -> Type -> Type -> Tc TEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  subsumptionCheck tenv2 e e'

-- When a variable is coming from the local stack to the data stack, we want to
-- know whether it's coming by move or by copy.

data HowCome = Move | Copy
  deriving (Eq)

-- Show instances for various types.

instance Show Expr where
  show e = case e of
    EPush tref val -> showTyped tref $ show val
    ECall tref name [] -> showTyped tref $ Text.unpack name
    ECall tref name params -> showTyped tref $ concat [
      Text.unpack name,
      "<",
      intercalate ", " (map show params),
      ">" ]
    ECat tref a b -> showTyped tref $ unwords' [show a, show b]
    EId tref -> showTyped tref $ ""
    EQuote tref expr -> showTyped tref $ "[ " ++ show expr ++ " ]"
    EGo tref name -> showTyped tref $ '&' : Text.unpack name
    ECome Move tref name -> showTyped tref $ '-' : Text.unpack name
    ECome Copy tref name -> showTyped tref $ '+' : Text.unpack name
    where
    showTyped (Just type_) x = "(" ++ x ++ " : " ++ show type_ ++ ")"
    showTyped Nothing x = x
    unwords' = unwords . filter (not . null)

instance Show Type where
  showsPrec p t = case t of
    TCon con -> shows con
    TVar (Just k) x -> showParen True $ shows x . showChar ':' . shows k
    TVar Nothing x -> shows x
    TConst x -> shows x
    TForall x k t' -> showParen True $ showChar '\x2200' . shows (TVar k x) . showString ". " . shows t'
    a `TApp` b -> showParen (p > appPrec) $ showsPrec appPrec a . showChar ' ' . showsPrec (appPrec + 1) b
    where
    appPrec = 1

instance Show TypeId where
  show (TypeId x) = 't' : show x

instance Show Con where
  show con = case con of
    CInt -> "int"
    CProd -> "pair"
    CVec -> "vec"
    CFun -> "fun"
    CPure -> "pure"
    CIO -> "io"
    CFail -> "fail"
    CJoin -> "join"

instance Show Kind where
  showsPrec p k = case k of
    KVal -> showString "*"
    KRho -> showString "\x03C1"
    KEff -> showString "\x03B5"
    KEffRho -> showString "\x0395"
    a `KFun` b -> showParen (p > funPrec) $ showsPrec (funPrec + 1) a . showString " \x2192 " . showsPrec funPrec b
    KVar x -> shows x
    where
    funPrec = 1

instance Show KindId where
  show (KindId x) = 'k' : show x

instance Show Val where
  show (VInt i) = show i

instance Show TEnv where
  show tenv = concat [
    "{ ",
    intercalate ", " $ concat [
      map (\ (t, t') -> show (TVar Nothing t) ++ " ~ " ++ show t') (Map.toList (envTvs tenv)),
      map (\ (k, k') -> show (KVar k) ++ " ~ " ++ show k') (Map.toList (envKvs tenv)),
      map (\ (t, k) -> show (TVar Nothing t) ++ " : " ++ show k) (Map.toList (envTks tenv)),
      map (\ (v, t) -> Text.unpack v ++ " : " ++ show t) (Map.toList (envVs tenv)) ],
    " }" ]

-- Utility operators for constructing types and kinds.

(.->) :: Type -> Type -> Type -> Type
(t1 .-> t2) e = cfun .$ t1 .$ t2 .$ e
infixr 4 .->

(.$) :: Type -> Type -> Type
(.$) = TApp
infixl 1 .$

(.*) :: Type -> Type -> Type
t1 .* t2 = cprod .$ t1 .$ t2
infixl 5 .*

(.|) :: Type -> Type -> Type
t1 .| t2 = cjoin .$ t1 .$ t2
infixr 4 .|

(..->) :: Kind -> Kind -> Kind
(..->) = KFun
infixr 4 ..->

-- The typechecker monad.

newtype Tc a = Tc { runTc :: IO (Either String a) }

instance Functor Tc where
  fmap f (Tc mx) = Tc $ do
    ex <- mx
    return $ case ex of
      Left x -> Left x
      Right x -> Right $ f x

instance Applicative Tc where
  pure = Tc . return . Right
  Tc mf <*> Tc mx = Tc $ do
    ef <- mf
    ex <- mx
    return $ case (ef, ex) of
      (Left f, _) -> Left f
      (_, Left x) -> Left x
      (Right f, Right x) -> Right $ f x

instance Monad Tc where
  return = Tc . return . Right
  Tc mx >>= f = Tc $ do
    ex <- mx
    case ex of
      Left x -> return $ Left x
      Right x -> runTc $ f x
  fail = Tc . return . Left

instance MonadFix Tc where
  mfix k = Tc $ do
    m <- newEmptyMVar
    a <- unsafeInterleaveIO (takeMVar m)
    ex <- runTc $ k a
    case ex of
      Left e -> return (Left e)
      Right x -> do
        putMVar m x
        return (Right x)

liftEither :: Either String a -> Tc a
liftEither = Tc . return

while :: [String] -> Tc a -> Tc a
while prefix action = Tc $ do
  ex <- runTc action
  return $ case ex of
    Left message -> Left $ unlines [unwords prefix, message]
    Right x -> Right x

-- Generating names and named things from a typing environment.

freshTv :: TEnv -> Tc Type
freshTv = fmap (TVar Nothing) . freshTypeId

freshTc :: TEnv -> Tc Type
freshTc = fmap TConst . freshTypeId

freshKv :: TEnv -> Tc Kind
freshKv = fmap KVar . freshKindId

freshTypeId :: TEnv -> Tc TypeId
freshTypeId tenv = do
  x <- Tc $ Right <$> readIORef (envCurrentType tenv)
  Tc $ Right <$> writeIORef (envCurrentType tenv) (succ x)
  return x

freshKindId :: TEnv -> Tc KindId
freshKindId tenv = do
  x <- Tc $ Right <$> readIORef (envCurrentKind tenv)
  Tc $ Right <$> writeIORef (envCurrentKind tenv) (succ x)
  return x

-- Parses an expression, for interactive testing purposes.

parse :: String -> Expr
parse s = case Parsec.parse (between spaces eof terms) "" s of
  Left message -> error $ show message
  Right result -> compose result
  where
  compose = foldl' (ECat Nothing) (EId Nothing)
  terms = many term
  term = choice [
    EQuote Nothing . compose <$> between (char '[' <* spaces) (char ']' <* spaces) terms,
    EGo Nothing <$> (char '&' *> name),
    ECome Copy Nothing <$> (char '+' *> name),
    ECome Move Nothing <$> (char '-' *> name),
    try $ do
      w <- word
      when (w == "]") parserZero
      return $ if all isDigit w
        then EPush Nothing (VInt (read w))
        else ECall Nothing (Text.pack w) [] ]
  name = Text.pack <$> word
  word = many1 (satisfy (not . isSpace .&& not . (== ']'))) <* spaces

(.&&) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&) = liftA2 (&&)
infixr 3 .&&
