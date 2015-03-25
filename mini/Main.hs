--------------------------------------------------------------------------------
-- Table of Contents
--------------------------------------------------------------------------------




-- Boilerplate
-- Data Types
-- Type Inference
-- Unification
--   Type Unification
--   Row Unification
-- Operations on Types and Kinds
--   Zonking
--   Substitution
--   Free Variables
--   Occurs Checks
-- Instantiation and Regeneralization
-- Subsumption Checking
-- Instance Generation
--
-- Test Suite
-- References
-- Typechecker Monad
-- Parsing
-- Typeclass Instances




--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------




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
import Data.Foldable (foldlM, foldrM)
import Data.Function
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts
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




--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------




-- This is the core language. It permits pushing values to the stack, invoking
-- definitions, and moving or copying values between the stack and local
-- variables.
--
-- It also permits empty programs and program concatenation, and together these
-- form a monoid over programs. The denotation of the concatenation of two
-- programs is the composition of the denotations of those two programs, which
-- is to say that there is a homomorphism from the syntactic monoid onto the
-- semantic monoid.

data Expr
  = EId !TRef
  | ECat !TRef !Expr !Expr
  | ECall !TRef !Name [Type]
  | EPush !TRef !Val
  | EGo !TRef !Name
  | ECome !HowCome !TRef !Name
  | EForall !TypeId !Expr
  deriving (Eq)

data HowCome = Move | Copy
  deriving (Eq)




-- This is the type language. It describes a system of conventional
-- Hindley-Milner types, with type constructors joined by type application, as
-- well as type variables and constants for constraint solving and instance
-- checking, respectively. It syntactically permits higher-ranked
-- quantification, though there are semantic restrictions on this, discussed in
-- the presentation of the inference algorithm. Type variables are annotated
-- with their kinds.

data Type
  = TCon !Con
  | TVar !Var
  | TConst !Var
  | TForall !Var !Type
  | !Type :@ !Type
  deriving (Eq)

data Var = Var !TypeId !Kind
  deriving (Eq)

newtype Con = Con Name
  deriving (Eq)

type TRef = Maybe Type

(.->) :: Type -> Type -> Type -> Type
(t1 .-> t2) e = "fun" :@ t1 :@ t2 :@ e
infixr 4 .->

infixl 1 :@

(.*) :: Type -> Type -> Type
t1 .* t2 = "prod" :@ t1 :@ t2
infixl 5 .*

(.|) :: Type -> Type -> Type
t1 .| t2 = "join" :@ t1 :@ t2
infixr 4 .|




-- A kind is the type of a type. Types with the "value" kind (*) are inhabited
-- by values; all other types are used only to enforce program invariants. These
-- consist of the "stack" kind (ρ), the "effect" kind (ε), the "effect row" kind
-- (Ε), and the "function" kind (κ → κ) used to describe type constructors.

data Kind
  = KVal
  | KRho
  | KEff
  | KEffRho
  | KFun !Kind !Kind
  deriving (Eq)

(..->) :: Kind -> Kind -> Kind
(..->) = KFun
infixr 4 ..->




-- Type variables are distinguished by globally unique identifiers. This makes
-- it easier to support capture-avoiding substitution on types.

newtype TypeId = TypeId { unTypeId :: Int }
  deriving (Enum, Eq, Ord)




-- A value is a literal instance of a type, which may be placed on the stack.

data Val
  = VInt !Int
  | VName !Name
  deriving (Eq)




-- Definitions are indexed by their names.

type Name = Text




-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What is the type of this type variable?
--  • What is the kind of this type variable?
--  • What is the type of this local variable?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TEnv = TEnv {
  envTvs :: Map TypeId Type,
  envTks :: Map TypeId Kind,
  envVs :: Map Name Type,
  envSigs :: Map Name Type,
  envCurrentType :: IORef TypeId }

emptyTEnv :: TEnv
emptyTEnv = TEnv {
  envTvs = Map.empty,
  envTks = Map.empty,
  envVs = Map.empty,
  envSigs = Map.empty,
  envCurrentType = currentTypeId }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}




--------------------------------------------------------------------------------
-- Type Inference
--------------------------------------------------------------------------------




-- To infer the types in a mutually-recursive binding group, the type of each
-- definition is inferred under the assumption that all signatures in the
-- binding group are correct, then the inferred type is checked against the
-- declared type.

inferTypes :: Map Name (Type, Expr) -> Expr -> Tc (Map Name (Type, Expr), Expr, Type)
inferTypes defs expr0 = do
  inferredDefs <- fmap Map.fromList . mapM go . Map.toList $ defs
  let sigs' = Map.map fst inferredDefs
  (expr1, scheme) <- inferType0 sigs' expr0
  let expr2 = quantifyExpr scheme expr1
  return (inferredDefs, expr2, scheme)
  where
  sigs = Map.map fst defs
  go (name, (scheme, expr)) = do
    (expr', scheme') <- inferType0 sigs expr
    instanceCheck scheme' scheme
    let expr'' = quantifyExpr scheme' expr'
    return (name, (scheme', expr''))




-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.
--
-- See: Instantiation and Regeneralization

inferType0 :: Map Name Type -> Expr -> Tc (Expr, Type)
inferType0 sigs expr = while ["inferring the type of", show expr] $ do
  rec (expr', t, tenvFinal) <- inferType tenvFinal emptyTEnv { envSigs = sigs } expr
  let regeneralized = regeneralize tenvFinal (zonkType tenvFinal t)
  return (zonkExpr tenvFinal expr', regeneralized)




-- We infer the type of an expression tree and annotate each terminal with the
-- inferred type as we go.

inferType :: TEnv -> TEnv -> Expr -> Tc (Expr, Type, TEnv)
inferType tenvFinal tenv0 expr0 = while ["inferring the type of", show expr0] $ case expr0 of

-- Pushing a value results in a stack with that value on top.

  EPush Nothing val -> do
    [a, e] <- fresh [KRho, KEffRho]
    (val', t, tenv1) <- inferVal tenvFinal tenv0 val
    let type_ = (a .-> a .* t) e
    let type' = zonkType tenvFinal type_
    return (EPush (Just type') val', type_, tenv1)

  ECall Nothing name [] -> inferCall tenvFinal tenv0 name

-- The type of the composition of two expressions is the composition of the
-- types of those expressions.

  ECat Nothing expr1 expr2 -> do
    (expr1', t1, tenv1) <- inferType' tenv0 expr1
    (expr2', t2, tenv2) <- inferType' tenv1 expr2
    (a, b, e1, tenv3) <- unifyFun tenv2 t1
    (c, d, e2, tenv4) <- unifyFun tenv3 t2
    tenv5 <- unifyType tenv4 b c
    tenv6 <- unifyType tenv5 e1 e2
    let type_ = (a .-> d) e1
    let type' = zonkType tenvFinal type_
    return (ECat (Just type') expr1' expr2', type_, tenv6)

-- The empty program is the identity function on stacks.

  EId Nothing -> do
    [a, e] <- fresh [KRho, KEffRho]
    let type_ = (a .-> a) e
    let type' = zonkType tenvFinal type_
    return (EId (Just type'), type_, tenv0)

-- A value going from the stack to the locals.

  EGo Nothing name -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* b .-> a) e
    let type' = zonkType tenvFinal type_
    return (EGo (Just type') name, type_, tenv0 { envVs = Map.insert name b (envVs tenv0) })

-- A value coming from the locals to the stack. If it's coming by moving, its
-- name is removed from the local scope. This relies on composition being
-- inferred in left-to-right order.

  ECome how Nothing name -> do
    [a, e] <- fresh [KRho, KEffRho]
    b <- case Map.lookup name (envVs tenv0) of
      Just t -> return t
      Nothing -> fail $ "unbound variable " ++ Text.unpack name
    let
      tenv1 = case how of
        Move -> tenv0 { envVs = Map.delete name (envVs tenv0) }
        Copy -> tenv0
    let type_ = (a .-> a .* b) e
    let type' = zonkType tenvFinal type_
    return (ECome how (Just type') name, type_, tenv1)

  _ -> fail $ unwords ["cannot infer type of already-annotated expression ", show expr0]

  where
  inferType' = inferType tenvFinal
  fresh = foldrM (\k ts -> (: ts) <$> freshTv tenv0 k) []

inferVal :: TEnv -> TEnv -> Val -> Tc (Val, Type, TEnv)
inferVal tenvFinal tenv0 val = case val of
  VInt{} -> return (val, "int", tenv0)
  VName name -> do
    (_, type_, tenv1) <- inferCall tenvFinal tenv0 name
    return (val, type_, tenv1)




-- Infers the type of a call.

inferCall :: TEnv -> TEnv -> Name -> Tc (Expr, Type, TEnv)
inferCall tenvFinal tenv0 name = case name of

-- Pure intrinsics.

  "add" -> do
    [a, e] <- fresh [KRho, KEffRho]
    let type_ = (a .* "int" .* "int" .-> a .* "int") e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)
  "com" -> do
    [a, b, c, d, e1, e2] <- fresh [KRho, KRho, KRho, KRho, KEffRho, KEffRho]
    let type_ = (a .* (b .-> c) e1 .* (c .-> d) e1 .-> a .* (b .-> d) e1) e2
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)
  "app" -> do
    [a, b, e] <- fresh [KRho, KRho, KEffRho]
    let type_ = (a .* (a .-> b) e .-> b) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)
  "quo" -> do
    [a, b, c, e1, e2] <- fresh [KRho, KVal, KRho, KEffRho, KEffRho]
    let type_ = (a .* b .-> a .* (c .-> c .* b) e1) e2
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

-- Vector intrinsics.

  "vec" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* b .-> a .* ("vec" :@ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "head" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* ("vec" :@ b) .-> a .* b) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "tail" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* ("vec" :@ b) .-> a .* ("vec" :@ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "cat" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* ("vec" :@ b) .* ("vec" :@ b) .-> a .* ("vec" :@ b)) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

-- Effectful intrinsics. Note that we allow the effect to be polymorphic in its
-- tail, so that effects can be composed.

  "say" -> do
    [a, e] <- fresh [KRho, KEffRho]
    let type_ = (a .* "int" .-> a) ("io" .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "abort" -> do
    [a, b, e] <- fresh [KRho, KRho, KEffRho]
    let type_ = (a .-> b) ("fail" .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

-- Unsafe intrinsics.

  "ref" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* b .-> a .* ("ptr" :@ b)) ("unsafe" .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "deref" -> do
    [a, b, e] <- fresh [KRho, KVal, KEffRho]
    let type_ = (a .* ("ptr" :@ b) .-> a .* b) ("unsafe" .| e)
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

  "unsafe" -> do
    [a, b, e] <- fresh [KRho, KRho, KEffRho]
    let type_ = (a .* (a .-> b) ("unsafe" .| e) .-> b) e
    let type' = zonkType tenvFinal type_
    return (ECall (Just type') name [], type_, tenv0)

-- The type of a definition is simply looked up in the environment.

  _ -> case Map.lookup name (envSigs tenv0) of
    Just t@TForall{} -> do
      (type_, params, tenv1) <- instantiatePrenex tenv0 t
      params' <- filterM (fmap (KVal ==) . typeKind) params
      let type' = zonkType tenvFinal type_
      return (ECall (Just type') name params', type_, tenv1)
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> fail $ "cannot infer type of " ++ show name

  where
  fresh = foldrM (\k ts -> (: ts) <$> freshTv tenv0 k) []




typeKind :: Type -> Tc Kind
typeKind t = case t of
  TCon con -> case con of
    "int" -> return $ KVal
    "fun" -> return $ KRho ..-> KRho ..-> KEffRho ..-> KVal
    "prod" -> return $ KRho ..-> KVal ..-> KRho
    "vec" -> return $ KVal ..-> KVal
    "ptr" -> return $ KVal ..-> KVal
    "unsafe" -> return KEff
    "pure" -> return KEff
    "io" -> return KEff
    "fail" -> return KEff
    "join" -> return $ KEff ..-> KEffRho ..-> KEffRho
    _ -> fail $ unwords ["cannot infer kind of", show con]
  TVar (Var _ k) -> return k
  TConst (Var _ k) -> return k
  TForall _ t' -> typeKind t'
  a :@ b -> do
    ka <- typeKind a
    case ka of
      KFun _ k -> return k
      _ -> fail $ unwords ["applying non-constructor", show a, "to type", show b]




--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------




----------------------------------------
-- Type Unification
----------------------------------------




-- There are two kinds of unification going on here: basic logical unification
-- for value types, and row unification for effect types.

unifyType :: TEnv -> Type -> Type -> Tc TEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> return tenv0
  (TVar (Var x _), t) -> unifyTv tenv0 x t
  (_, TVar{}) -> commute
  -- FIXME: Unify the kinds here?
  (a, TForall (Var x k) t) -> do
    (b, _, tenv1) <- instantiate tenv0 x k t
    unifyType tenv1 a b
  (TForall{}, _) -> commute

-- The occurs check here prevents us from unifying rows with a common tail and a
-- distinct prefix, which could fail to terminate because the algorithm
-- generates fresh type variables.
--
-- See: Row Unification

  ("join" :@ l :@ r, s) -> do
    ms <- rowIso tenv0 l s
    case ms of
      Just ("join" :@ _ :@ s', substitution, tenv1) -> case substitution of
        Just (x, t)
          | occurs tenv0 x (effectTail r)
          -> fail $ unwords ["cannot unify effects", show t1, "and", show t2]
          | otherwise -> let
            tenv2 = tenv1 { envTvs = Map.insert x t (envTvs tenv1) }
            in unifyType tenv2 r s'
        Nothing -> unifyType tenv1 r s'

      -- HACK: Duplicates the remaining cases.
      _ -> case (t1, t2) of
        (a :@ b, c :@ d) -> do
          tenv1 <- unifyType tenv0 a c
          unifyType tenv1 b d
        _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

  (_, "join" :@ _ :@ _) -> commute

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and effect type constructors.

  (a :@ b, c :@ d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d

  _ -> fail $ unwords ["cannot unify types", show t1, "and", show t2]

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  commute = unifyType tenv0 t2 t1
  effectTail ("join" :@ _ :@ a) = effectTail a
  effectTail t = t




-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.
--
-- See: Occurs Checks

unifyTv :: TEnv -> TypeId -> Type -> Tc TEnv
unifyTv tenv0 x t = case t of
  TVar (Var y _) | x == y -> return tenv0
  TVar{} -> declare
  _ -> if occurs tenv0 x t then fail "occurs check" else declare
  where
  declare = case Map.lookup x (envTvs tenv0) of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> return tenv0 { envTvs = Map.insert x t (envTvs tenv0) }




-- A convenience function for unifying a type with a function type.

unifyFun :: TEnv -> Type -> Tc (Type, Type, Type, TEnv)
unifyFun tenv0 t = case t of
  "fun" :@ a :@ b :@ e -> return (a, b, e, tenv0)
  _ -> do
    a <- freshTv tenv0 KRho
    b <- freshTv tenv0 KRho
    e <- freshTv tenv0 KEffRho
    tenv1 <- unifyType tenv0 t ((a .-> b) e)
    return (a, b, e, tenv1)





----------------------------------------
-- Row Unification
----------------------------------------




-- Row unification is essentially unification of sets. The row-isomorphism
-- operation (as described in [1]) takes an effect label and an effect row, and
-- asserts that the row can be rewritten to begin with that label under some
-- substitution. It returns the substitution and the tail of the rewritten
-- row. The substitution is always either empty (∅) or a singleton substitution
-- (x ↦ τ).

rowIso :: TEnv -> Type -> Type -> Tc (Maybe (Type, Maybe (TypeId, Type), TEnv))

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 lin rin@("join" :@ l :@ _)
  | l == lin = return $ Just (rin, Nothing, tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l ("join" :@ l' :@ r)
  | l /= l' = do
  ms <- rowIso tenv0 l r
  return $ case ms of
    Just (r', substitution, tenv1) -> Just (l .| l' .| r', substitution, tenv1)
    Nothing -> Nothing

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail.

rowIso tenv0 l (TVar (Var a _)) = do
  b <- freshTv tenv0 KEffRho
  let res = l .| b
  return $ Just (res, Just (a, res), tenv0)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ = return Nothing




--------------------------------------------------------------------------------
-- Operations on Types and Kinds
--------------------------------------------------------------------------------



----------------------------------------
-- Zonking
----------------------------------------




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
    TVar (Var x k) -> case Map.lookup x (envTvs tenv0) of
      Just (TVar (Var x' _)) | x == x' -> TVar (Var x k)
      Just t' -> recur t'
      Nothing -> t
    TConst{} -> t
    TForall (Var x k) t'
      -> TForall (Var x k) $ zonkType tenv0 { envTvs = Map.delete x (envTvs tenv0) } t'
    a :@ b -> recur a :@ recur b




-- Zonking an expression zonks all the annotated types of the terms. This could
-- be done more efficiently by sharing type references and updating them
-- impurely, but this implementation is easier to get right and understand.

zonkExpr :: TEnv -> Expr -> Expr
zonkExpr tenv0 = recur
  where
  recur expr = case expr of
    EPush tref val -> EPush (zonkTRef tref) val
    ECall tref name params -> ECall (zonkTRef tref) name (map (zonkType tenv0) params)
    ECat tref e1 e2 -> ECat (zonkTRef tref) (recur e1) (recur e2)
    EId tref -> EId (zonkTRef tref)
    EGo tref name -> EGo (zonkTRef tref) name
    ECome how tref name -> ECome how (zonkTRef tref) name
    EForall{} -> error "cannot zonk generic expression"
  zonkTRef = fmap (zonkType tenv0)




----------------------------------------
-- Substitution
----------------------------------------




-- Capture-avoiding substitution of a type variable α with a type τ throughout a
-- type σ, [α ↦ τ]σ.

replaceTv :: TEnv -> TypeId -> Type -> Type -> Tc Type
replaceTv tenv0 x a = recur
  where
  recur t = case t of
    TForall (Var x' k) t'
      | x == x' -> return t
      | x' `Set.notMember` freeTvs t' -> TForall (Var x' k) <$> recur t'
      | otherwise -> do
        z <- freshTypeId tenv0
        t'' <- replaceTv tenv0 x' (TVar (Var z k)) t'
        TForall (Var z k) <$> recur t''
    TVar (Var x' _) | x == x' -> return a
    m :@ n -> (:@) <$> recur m <*> recur n
    _ -> return t




replaceTvExpr :: TEnv -> TypeId -> Type -> Expr -> Tc Expr
replaceTvExpr tenv x a = recur
  where
  recur expr = case expr of
    EId tref -> EId <$> go' tref
    ECat tref e1 e2 -> ECat <$> go' tref <*> recur e1 <*> recur e2
    ECall tref name args -> ECall <$> go' tref <*> pure name <*> mapM go args
    EPush tref val -> EPush <$> go' tref <*> pure val
    EGo tref name -> EGo <$> go' tref <*> pure name
    ECome how tref name -> ECome how <$> go' tref <*> pure name
    EForall x' _
      | x == x' -> return expr
      | otherwise -> error "substituting in a generic expression should not require capture-avoidance"
  go' (Just t) = Just <$> go t
  go' Nothing = return Nothing
  go t = replaceTv tenv x a t




----------------------------------------
-- Free Variables
----------------------------------------




-- The free variables of a type are those not bound by any quantifier.

freeTvs :: Type -> Set TypeId
freeTvs = Set.fromList . Map.keys . freeTvks

freeTvks :: Type -> Map TypeId Kind
freeTvks t = case t of
  TCon{} -> Map.empty
  TVar (Var x k) -> Map.singleton x k
  TConst{} -> Map.empty
  TForall (Var x _) t' -> Map.delete x (freeTvks t')
  a :@ b -> Map.union (freeTvks a) (freeTvks b)




----------------------------------------
-- Occurs Checks
----------------------------------------




-- We need to be able to count occurrences of a type variable in a type for two
-- reasons: to prevent infinite types (the "occurs check"), and to determine
-- whether a stack variable can be generalized to a higher rank.

occurrences :: TEnv -> TypeId -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TCon{} -> 0
    TVar (Var y _) -> case Map.lookup y (envTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TConst{} -> 0
    TForall (Var x' _) t' -> if x == x' then 0 else recur t'
    a :@ b -> recur a + recur b

occurs :: TEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0




--------------------------------------------------------------------------------
-- Instantiation and Regeneralization
--------------------------------------------------------------------------------




-- To instantiate a type scheme, we simply replace all quantified variables with
-- fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order.

instantiate :: TEnv -> TypeId -> Kind -> Type -> Tc (Type, Type, TEnv)
instantiate tenv0 x k t = do
  ia <- freshTypeId tenv0
  let a = TVar (Var ia k)
  replaced <- replaceTv tenv0 x a t
  return (replaced, a, tenv0 { envTks = Map.insert ia k (envTks tenv0) })




-- When generating an instantiation of a generic definition, we only want to
-- instantiate the rank-1 quantifiers; all other quantifiers are irrelevant.

instantiatePrenex :: TEnv -> Type -> Tc (Type, [Type], TEnv)
instantiatePrenex tenv0 q@(TForall (Var x k) t) = while ["instantiating", show q] $ do
  (t', a, tenv1) <- instantiate tenv0 { envTks = Map.insert x k (envTks tenv0) } x k t
  (t'', as, tenv2) <- instantiatePrenex tenv1 t'
  return (t'', a : as, tenv2)
instantiatePrenex tenv0 t = return (t, [], tenv0)




-- Because all functions are polymorphic with respect to the part of the stack
-- they don't touch, all words of order n can be regeneralized to words of rank
-- n with respect to the stack-kinded type variables.
--
-- This means that if a stack-kinded (ρ) type variable occurs only twice in a
-- type, in the bottommost position on both sides of a function arrow, then its
-- scope can be reduced to only that function arrow by introducing a
-- higher-ranked quantifier. For example, the type of "map":
--
--     map :: ∀ρσαβ. ρ × vector α × (σ × α → σ × β) → ρ × vector β
--
-- Can be regeneralized like so:
--
--     map :: ∀ραβ. ρ × vector α × (∀σ. σ × α → σ × β) → ρ × vector β
--
-- In order to correctly regeneralize a type, it needs to contain no
-- higher-ranked quantifiers.
--
-- This is a more conservative rule than used in [3].

regeneralize :: TEnv -> Type -> Type
regeneralize tenv t = let
  (t', vars) = runWriter $ go t
  in foldr (uncurry ((TForall .) . Var)) t' $ foldr (deleteBy ((==) `on` fst)) (Map.toList (freeTvks t')) vars
  where
  go :: Type -> Writer [(TypeId, Kind)] Type
  go t' = case t' of
    TCon "fun" :@ a :@ b :@ e
      | TVar (Var c k) <- bottommost a
      , TVar (Var d _) <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [(c, k)]
        a' <- go a
        b' <- go b
        e' <- go e
        return $ TForall (Var c k) ((a' .-> b') e')
    TCon "prod" :@ a :@ b -> do
      a' <- go a
      b' <- go b
      return $ "prod" :@ a' :@ b'
    TForall{} -> error "cannot regeneralize higher-ranked type"
    a :@ b -> (:@) <$> go a <*> go b
    _ -> return t'
  bottommost ("prod" :@ a :@ _) = bottommost a
  bottommost a = a




--------------------------------------------------------------------------------
-- Subsumption Checking
--------------------------------------------------------------------------------




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




-- Skolemization replaces quantified type variables with type constants.

skolemize :: TEnv -> Type -> Tc (Set TypeId, Type)
skolemize tenv0 t = case t of
  TForall (Var x k) t' -> do
    c <- freshTypeId tenv0
    let tenv1 = tenv0 { envTvs = Map.insert x (TConst (Var c k)) (envTvs tenv0) }
    (c', t'') <- skolemize tenv1 (zonkType tenv1 t')
    return (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  "fun" :@ a :@ b :@ e -> do
    (ids, b') <- skolemize tenv0 b
    return (ids, (a .-> b') e)
  _ -> return (Set.empty, t)




-- Subsumption checking is largely the same as unification, except for the fact
-- that a function type is contravariant in its input type.

subsumptionCheck :: TEnv -> Type -> Type -> Tc TEnv
subsumptionCheck tenv0 (TForall (Var x k) t) t2 = do
  (t1, _, tenv1) <- instantiate tenv0 x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 ("fun" :@ a :@ b :@ e) = do
  (a', b', e', tenv1) <- unifyFun tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 ("fun" :@ a' :@ b' :@ e') t2 = do
  (a, b, e, tenv1) <- unifyFun tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'
subsumptionCheck tenv0 t1 t2 = unifyType tenv0 t1 t2

subsumptionCheckFun :: TEnv -> Type -> Type -> Type -> Type -> Type -> Type -> Tc TEnv
subsumptionCheckFun tenv0 a b e a' b' e' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  tenv2 <- subsumptionCheck tenv1 b b'
  subsumptionCheck tenv2 e e'




--------------------------------------------------------------------------------
-- Instance Generation
--------------------------------------------------------------------------------




-- In order to support unboxed generics, for every call site of a generic
-- definition in a program, we produce a specialized instantiation of the
-- definition with the value-kinded type parameters set to the given type
-- arguments. This is transitive: if a generic definition calls another generic
-- definition with one of its own generic type parameters as a type argument,
-- then an instantiation must also be generated of the called definition.

collectInstantiations :: TEnv -> Map Name (Type, Expr) -> Expr -> Tc (Map Name (Type, Expr), Expr)
collectInstantiations tenv defs0 expr0 = do

-- We first enqueue all the instantiation sites reachable from the top level of
-- the program, and any non-generic definitions.

  (expr1, q0) <- go emptyQueue expr0
  (defs1, q1) <- foldrM
    (\ (name, (type_, expr)) (acc, q) -> do
      (expr', q') <- go q expr
      return ((name, (type_, expr')) : acc, q'))
    ([], q0)
    (Map.toList defs0)

-- Next, we process the queue. Doing so may enqueue new instantiation sites for
-- processing; however, this is guaranteed to halt because the number of actual
-- instantiations is finite.

  defs2 <- processQueue q1 (Map.fromList defs1)
  return (defs2, expr1)
  where
  go :: Queue (Name, [Type]) -> Expr -> Tc (Expr, Queue (Name, [Type]))
  go q0 expr = case expr of
    EId{} -> return (expr, q0)
    ECat tref a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      return (ECat tref a' b', q2)
    ECall tref name args -> return (ECall tref (mangleName name args) [], enqueue (name, args) q0)
    EPush{} -> return (expr, q0)
    EGo{} -> return (expr, q0)
    ECome{} -> return (expr, q0)
    -- If the definition is generic, we simply ignore it; we won't find any
    -- instantiations in it, because it's not instantiated, itself!
    EForall{} -> return (expr, q0)
  processQueue :: Queue (Name, [Type]) -> Map Name (Type, Expr) -> Tc (Map Name (Type, Expr))
  processQueue q defs = case dequeue q of
    Nothing -> return defs
    Just ((name, args), q') -> let
      mangled = mangleName name args
      in case Map.lookup mangled defs of
        Just{} -> processQueue q' defs
        Nothing -> case Map.lookup name defs of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' defs
          Just (type_, expr) -> do
            expr' <- instantiateExpr tenv expr args
            (expr'', q'') <- go q' expr'
            processQueue q'' (Map.insert mangled (type_, expr'') defs)




-- Names are mangled according to the local C++ mangling convention. This is a
-- silly approximation of the IA-64 convention for testing purposes.

mangleName :: Name -> [Type] -> Name
mangleName name args = case args of
  [] -> prefix <> lengthEncode name
  _ -> prefix <> lengthEncode name <> "I" <> Text.concat (map mangleType args) <> "E"
  where
  prefix = "_Z"

mangleType :: Type -> Name
mangleType ("fun" :@ _ :@ _) = "PFvv"
mangleType ("ptr" :@ a) = Text.concat ["P", mangleType a]
mangleType (TCon (Con con)) = case con of
  "int" -> "i"
  _ -> lengthEncode con
mangleType ("prod" :@ a :@ b) = Text.concat $ map mangleType [a, b]
mangleType _ = "?"

lengthEncode :: Name -> Name
lengthEncode name = Text.pack (show (Text.length name)) <> name




-- A generic queue with amortized O(1) enqueue/dequeue.

data Queue a = Queue [a] [a]

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue i (x : o)) = Just (x, Queue i o)
dequeue (Queue i@(_ : _) []) = dequeue (Queue [] (reverse i))
dequeue (Queue [] []) = Nothing

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x : i) o

emptyQueue :: Queue a
emptyQueue = Queue [] []

queueFromList :: [a] -> Queue a
queueFromList = Queue [] . reverse




-- Instantiates a generic expression with the given type arguments.

instantiateExpr :: TEnv -> Expr -> [Type] -> Tc Expr
instantiateExpr tenv = foldlM go
  where
  go (EForall x expr) arg = replaceTvExpr tenv x arg expr
  go _ _ = error "instantiateExpr: wrong number of type parameters"




-- Copies the top-level generic value-kinded type quantifiers from a polytype to
-- an expression, thereby making the expression generic, e.g.:
--
--     ∀α:ρ. ∀β:*. ∀γ:Ε. (α × β → α × β × β) Ε    dup
--
--     Λβ:*. dup

quantifyExpr :: Type -> Expr -> Expr
quantifyExpr (TForall (Var x KVal) t) e = EForall x (quantifyExpr t e)
quantifyExpr (TForall _ t) e = quantifyExpr t e
quantifyExpr _ e = e




--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------




spec :: Spec
spec = do
  describe "zonking" $ do
    it "does nothing to free type variables"
      $ assertEqual "" (zonkType emptyTEnv va) va
    it "substitutes one level"
      $ assertEqual "" (zonkType emptyTEnv { envTvs = Map.singleton ia vb } va) vb
    it "substitutes multiple levels"
      $ assertEqual ""
        (zonkType emptyTEnv { envTvs = Map.fromList [(ia, vb), (ib, "int")] } va)
        "int"
  describe "type inference" $ do
    it "gives the identity type for the empty program"
      $ testScheme (inferEmpty (parse ""))
      $ fr $ fe $ (vr .-> vr) ve
    it "gives the composed type from simple composition"
      $ testScheme (inferEmpty (parse "1 2 add"))
      $ fr $ fe $ (vr .-> vr .* "int") ve
    it "gives the composed type for higher-order composition"
      $ testScheme (inferEmpty (parse "1 quo 2 quo com \\add com app"))
      $ fr $ fe $ (vr .-> vr .* "int") ve
    it "deduces simple side effects"
      $ testScheme (inferEmpty (parse "1 say"))
      $ fr $ fe $ (vr .-> vr) ("io" .| ve)
    it "deduces higher-order side effects"
      $ testScheme (inferEmpty (parse "1 \\say app"))
      $ fr $ fe $ (vr .-> vr) ("io" .| ve)
    it "removes effects with an effect handler"
      $ testDefs
        (Map.singleton "q0" (fr $ fe $ (vr .-> vr .* "int") ("unsafe" .| ve), parse "1 ref deref"))
        (parse "\\q0 unsafe")
        $ fr $ fe $ (vr .-> vr .* "int") ve
    it "fails when missing an effect annotation"
      $ testFail $ inferTypes
        (Map.singleton "evil" (fr $ fe $ (vr .* "int" .-> vr) ve, parse "say"))
        (parse "evil")
    it "fails on basic type mismatches"
      $ testFail (inferEmpty (parse "1 [add] add"))
    it "correctly copies from a local"
      $ testScheme (inferEmpty (parse "1 &x +x"))
      $ fr $ fe $ (vr .-> vr .* "int") ve
    it "correctly copies multiple times from a local"
      $ testScheme (inferEmpty (parse "1 &x +x +x"))
      $ fr $ fe $ (vr .-> vr .* "int" .* "int") ve
    it "correctly moves from a local"
      $ testScheme (inferEmpty (parse "1 &x -x"))
      $ fr $ fe $ (vr .-> vr .* "int") ve
    it "fails when moving from a moved-from local"
      $ testFail (inferEmpty (parse "1 &x -x -x"))
    it "fails when copying from a moved-from local"
      $ testFail (inferEmpty (parse "1 &x -x +x"))
  describe "definitions" $ do
    it "checks the type of a definition" $ let
      idType = fr $ fa $ fe $ (vr .* va .-> vr .* va) ve
      in testDefs (Map.singleton "id" (idType, parse "&x -x")) (parse "id") idType
  where
  inferEmpty expr = snd <$> inferType0 Map.empty expr
  testFail action = do
    result <- runTc action
    assert $ either (const True) (const False) result
  testScheme inference expected = do
    result <- runTc $ do
      scheme <- inference
      instanceCheck scheme expected
    either assertFailure (const (return ())) result
  testDefs defs expr scheme
    = testScheme ((\ (_, _, t) -> t) <$> inferTypes defs expr) scheme
  ir = TypeId 0
  vr = TVar (Var ir kr)
  ia = TypeId 1
  va = TVar (Var ia kv)
  ib = TypeId 2
  vb = TVar (Var ib kv)
  ie = TypeId 3
  ve = TVar (Var ie ke)
  kr = KRho
  kv = KVal
  ke = KEffRho
  fr = TForall (Var ir kr)
  fa = TForall (Var ia kv)
  fe = TForall (Var ie ke)




--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------




-- [1] D. Leijen. Extensible Records with Scoped Labels.
--
-- [2] D. Leijen. Koka: Programming with Row-polymorphic Effect Types.
--
-- [3] C. Diggins. Simple Type Inference for Higher-order Stack-oriented Languages.




--------------------------------------------------------------------------------
-- Typechecker Monad
--------------------------------------------------------------------------------




-- We introduce a typechecker monad for handling failure and the generation of
-- fresh identifiers. Notably, the typechecker monad does not manage the state
-- of type environments; these are threaded explicitly. This is more verbose,
-- but it makes it more apparent which typing environment is in use.

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

freshTv :: TEnv -> Kind -> Tc Type
freshTv tenv k = TVar <$> (Var <$> freshTypeId tenv <*> pure k)

freshTypeId :: TEnv -> Tc TypeId
freshTypeId tenv = do
  x <- Tc $ Right <$> readIORef (envCurrentType tenv)
  Tc $ Right <$> writeIORef (envCurrentType tenv) (succ x)
  return x




--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------




parse :: String -> Expr
parse s = case Parsec.parse (between spaces eof terms) "" s of
  Left message -> error $ show message
  Right result -> compose result
  where
  compose = foldl' (ECat Nothing) (EId Nothing)
  terms = many term
  term = choice [
    EPush Nothing . VName <$> (char '\\' *> name),
    EGo Nothing <$> (char '&' *> name),
    ECome Copy Nothing <$> (char '+' *> name),
    ECome Move Nothing <$> (char '-' *> name),
    try $ do
      w <- word
      return $ if all isDigit w
        then EPush Nothing (VInt (read w))
        else ECall Nothing (Text.pack w) [] ]
  name = Text.pack <$> word
  word = many1 (satisfy (not . isSpace)) <* spaces




--------------------------------------------------------------------------------
-- Typeclass Instances
--------------------------------------------------------------------------------




instance IsString Con where
  fromString = Con . Text.pack

instance IsString Type where
  fromString = TCon . fromString




instance Show Con where
  show (Con con) = Text.unpack con

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
    EGo tref name -> showTyped tref $ '&' : Text.unpack name
    ECome Move tref name -> showTyped tref $ '-' : Text.unpack name
    ECome Copy tref name -> showTyped tref $ '+' : Text.unpack name
    EForall x e' -> concat ["\x039B", show x, ". ", show e']
    where
    showTyped (Just type_) x = "(" ++ x ++ " : " ++ show type_ ++ ")"
    showTyped Nothing x = x
    unwords' = unwords . filter (not . null)

instance Show Kind where
  showsPrec p k = case k of
    KVal -> showString "*"
    KRho -> showString "\x03C1"
    KEff -> showString "\x03B5"
    KEffRho -> showString "\x0395"
    a `KFun` b -> showParen (p > funPrec) $ showsPrec (funPrec + 1) a . showString " \x2192 " . showsPrec funPrec b
    where
    funPrec = 1

instance Show TEnv where
  show tenv = concat [
    "{ ",
    intercalate ", " $ concat [
      map (\ (t, t') -> show (TVar (Var t KVal)) ++ " ~ " ++ show t') (Map.toList (envTvs tenv)),
      map (\ (t, k) -> show (TVar (Var t KVal)) ++ " : " ++ show k) (Map.toList (envTks tenv)),
      map (\ (v, t) -> Text.unpack v ++ " : " ++ show t) (Map.toList (envVs tenv)) ],
    " }" ]

instance Show Type where
  showsPrec p t = case t of
    TCon con -> shows con
    TVar v -> shows v
    TConst v -> shows v
    TForall v t' -> showParen True $ showChar '\x2200' . shows v . showString ". " . shows t'
    a :@ b -> showParen (p > appPrec) $ showsPrec appPrec a . showChar ' ' . showsPrec (appPrec + 1) b
    where
    appPrec = 1

instance Show Var where
  showsPrec _ (Var x k) = showParen True $ shows x . showChar ':' . shows k

instance Show TypeId where
  show (TypeId x) = 't' : show x

instance Show Val where
  show v = case v of
    VInt i -> show i
    VName name -> Text.unpack name
