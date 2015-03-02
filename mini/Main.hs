{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable (foldrM)
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Test.HUnit

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
      $ testScheme (inferType0 Map.empty (parse ""))
      $ TForall ia (TForall ib ((va .-> va) vb))
    it "gives the composed type from simple composition"
      $ testScheme (inferType0 Map.empty (parse "1 2 add"))
      $ TForall ia (TForall ib ((va .-> va .* TCon CInt) vb))
    it "deduces simple side effects"
      $ testScheme (inferType0 Map.empty (parse "1 say"))
      $ TForall ia (TForall ib ((va .-> va) (cio .| vb)))
  where
  testScheme inference expected = either assertFailure (const (return ())) $ do
    (_, scheme, _) <- inference
    instanceCheck scheme expected

  ia = TypeId 0
  va = TVar ia
  ib = TypeId 1
  vb = TVar ib

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

type TRef = Maybe Type

data Type

  -- Type constructor.
  = TCon Con

  -- Type variable, used during unification.
  | TVar TypeId

  -- Skolem constant, used during instance checking.
  | TConst TypeId

  -- Quantified type, used for both prenex and higher-rank polymorphism.
  | TForall TypeId Type

  -- Application of type constructor to type.
  | TApp Type Type

  deriving (Eq, Show)

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

cint, cfun, cprod, cpure, cio, cfail, cjoin :: Type
cint = TCon CInt
cfun = TCon CFun
cprod = TCon CProd
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
  | KVar KindId

  -- Kind of type constructors.
  | KFun Kind Kind

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

  -- The current state of type and kind ID generation.
  envCurrentType :: TypeId,
  envCurrentKind :: KindId }

-- An empty typing environment.

emptyTEnv :: TEnv
emptyTEnv = TEnv {
  envTvs = Map.empty,
  envTks = Map.empty,
  envKvs = Map.empty,
  envVs = Map.empty,
  envSigs = Map.empty,
  envCurrentType = TypeId 0,
  envCurrentKind = KindId 0 }

-- To infer the types in a mutually-recursive binding group, the type of each
-- definition is inferred under the assumption that all signatures in the
-- binding group are correct, then the inferred type is checked against the
-- declared type.

inferTypes :: Map Name (Type, Expr) -> Either String (Map Name (Type, Expr))
inferTypes defs = fmap Map.fromList . mapM go . Map.toList $ defs
  where
  sigs = Map.map fst defs
  go (name, (scheme, expr)) = do
    (expr', scheme', _) <- inferType0 sigs expr
    instanceCheck scheme' scheme
    Right (name, (scheme, expr'))

-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0 :: Map Name Type -> Expr -> Either String (Expr, Type, Kind)
inferType0 sigs expr = do
  (expr', t, tenv1) <- inferType emptyTEnv { envSigs = sigs } expr
  let zonkedType = zonkType tenv1 t
  let zonkedExpr = zonkExpr tenv1 expr'
  (kind, tenv2) <- inferKind tenv1 zonkedType
  let regeneralized = regeneralize tenv2 zonkedType
  tenv3 <- defaultKinds tenv2 kind
  let zonkedKind = zonkKind tenv3 kind
  Right (zonkedExpr, regeneralized, zonkedKind)

-- The default kind of a type is the value kind.

defaultKinds :: TEnv -> Kind -> Either String TEnv
defaultKinds tenv0 = foldrM (\ x tenv -> unifyKind tenv (KVar x) KVal) tenv0 . Set.toList . freeKvs

-- Kind inference lets us distinguish types from type constructors, and value
-- types from effect types.

inferKind :: TEnv -> Type -> Either String (Kind, TEnv)
inferKind tenv0 t = case t of
  TCon con -> Right (case con of
    CInt -> KVal
    CFun -> KRho ..-> KRho ..-> KEffRho ..-> KVal
    CProd -> KRho ..-> KVal ..-> KRho
    CPure -> KEff
    CIO -> KEff
    CFail -> KEff
    CJoin -> KEff ..-> KEffRho ..-> KEffRho, tenv0)
  TVar x -> case Map.lookup x (envTks tenv0) of
    Just k' -> Right (k', tenv0)
    Nothing -> let
      (k', tenv1) = freshKv tenv0
      in Right (k', tenv1 { envTks = Map.insert x k' (envTks tenv1) })
  TConst{} -> Left "cannot infer kind of skolem constant"
  TForall tv t' -> do
    (k1, _) <- inferKind
      (let
       (a, tenv') = freshKv tenv0
       in tenv' { envTks = Map.insert tv a (envTks tenv') })
      t'
    tenv1 <- unifyKind tenv0 k1 KVal
    Right (k1, tenv1)
  t1 `TApp` t2 -> do
    (k1, tenv1) <- inferKind tenv0 t1
    (k2, tenv2) <- inferKind tenv1 t2
    let (ka, tenv3) = freshKv tenv2
    let (kb, tenv4) = freshKv tenv3
    tenv5 <- unifyKind tenv4 k1 (ka ..-> kb)
    tenv6 <- unifyKind tenv5 k2 ka
    Right (kb, tenv6)

-- Kind unification proceeds similarly to type unification.

unifyKind :: TEnv -> Kind -> Kind -> Either String TEnv
unifyKind tenv0 k1 k2 = case (k1, k2) of
  _ | k1 == k2 -> Right tenv0
  (KVar x, t) -> unifyKv tenv0 x t
  (_, KVar{}) -> commute
  (a `KFun` b, c `KFun` d) -> do
    tenv1 <- unifyKind tenv0 a c
    unifyKind tenv1 b d
  _ -> Left $ unwords ["cannot unify kinds", show k1, "and", show k2]
  where
  commute = unifyKind tenv0 k2 k1

-- Inferring the type of an expression is straightforward. Most of these cases
-- simply encode the type signatures of various intrinsic functions.

inferType :: TEnv -> Expr -> Either String (Expr, Type, TEnv)
inferType tenv0 expr0 = case expr0 of

-- Pushing a value results in a stack with that value on top.

  EPush Nothing val -> let
    (val', t, tenv1) = inferVal tenv0 val
    (a, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .-> a .* t) e
    in Right (EPush (Just type_) val', type_, tenv3)

-- Pure intrinsics.

  ECall Nothing name@"add" [] -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .* cint .* cint .-> a .* cint) e
    in Right (ECall (Just type_) name [], type_, tenv2)
  ECall Nothing name@"cat" [] -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (c, tenv3) = freshTv tenv2
    (d, tenv4) = freshTv tenv3
    (e1, tenv5) = freshTv tenv4
    (e2, tenv6) = freshTv tenv5
    type_ = (a .* (b .-> c) e1 .* (c .-> d) e1 .-> a .* (b .-> d) e1) e2
    in Right (ECall (Just type_) name [], type_, tenv6)
  ECall Nothing name@"app" [] -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .* (a .-> b) e .-> b) e
    in Right (ECall (Just type_) name [], type_, tenv3)
  ECall Nothing name@"quo" [] -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (c, tenv3) = freshTv tenv2
    (e1, tenv4) = freshTv tenv3
    (e2, tenv5) = freshTv tenv4
    type_ = (a .* b .-> a .* (c .-> c .* b) e1) e2
    in Right (ECall (Just type_) name [], type_, tenv5)

-- Effectful intrinsics. Note that we allow the effect to be polymorphic in its
-- tail, so that effects can be composed.

  ECall Nothing name@"say" [] -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .* cint .-> a) (cio .| e)
    in Right (ECall (Just type_) name [], type_, tenv2)

  ECall Nothing name@"abort" [] -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .-> b) (cfail .| e)
    in Right (ECall (Just type_) name [], type_, tenv3)

-- The type of a definition is simply looked up in the environment.

  ECall Nothing name [] -> case Map.lookup name (envSigs tenv0) of
    Just (TForall x t) -> let
      (type_, param, tenv1) = instantiate tenv0 x t
      in Right (ECall (Just type_) name [param], type_, tenv1)
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> Left $ "cannot infer type of " ++ show name

-- The type of the composition of two expressions is the composition of the
-- types of those expressions.

  ECat Nothing expr1 expr2 -> do
    (expr1', t1, tenv1) <- inferType tenv0 expr1
    (expr2', t2, tenv2) <- inferType tenv1 expr2
    (a, b, e1, tenv3) <- unifyFun tenv2 t1
    (c, d, e2, tenv4) <- unifyFun tenv3 t2
    tenv5 <- unifyType tenv4 b c
    tenv6 <- unifyType tenv5 e1 e2
    let type_ = (a .-> d) e1
    Right (ECat (Just type_) expr1' expr2', type_, tenv6)

-- The empty program is the identity function on stacks.

  EId Nothing -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .-> a) e
    in Right (EId (Just type_), type_, tenv2)

-- A quoted expression is pushed rather than being evaluated.

  EQuote Nothing expr -> do
    let (a, tenv1) = freshTv tenv0
    (expr', b, tenv2) <- inferType tenv1 expr
    let (e, tenv3) = freshTv tenv2
    let type_ = (a .-> a .* b) e
    Right (EQuote (Just type_) expr', type_, tenv3)

-- A value going from the stack to the locals.

  EGo Nothing name -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .* b .-> a) e
    in Right (EGo (Just type_) name, type_, tenv3 { envVs = Map.insert name b (envVs tenv3) })

-- A value coming from the locals to the stack. If it's coming by moving, its
-- name is removed from the local scope. This relies on composition being
-- inferred in left-to-right order.

  ECome how Nothing name -> do
    let (a, tenv1) = freshTv tenv0
    b <- case Map.lookup name (envVs tenv1) of
      Just t -> Right t
      Nothing -> Left $ "unbound variable " ++ Text.unpack name
    let
      tenv2 = case how of
        Move -> tenv1 { envVs = Map.delete name (envVs tenv1) }
        Copy -> tenv1
    let (e, tenv3) = freshTv tenv2
    let type_ = (a .-> a .* b) e
    Right (ECome how (Just type_) name, type_, tenv3)
  _ -> Left $ "cannot infer type of already-inferred expression " ++ show expr0

-- A convenience function for unifying a type with a function type.

unifyFun :: TEnv -> Type -> Either String (Type, Type, Type, TEnv)
unifyFun tenv0 t = case t of
  TCon CFun `TApp` a `TApp` b `TApp` e -> Right (a, b, e, tenv0)
  _ -> do
    let (a, tenv1) = freshTv tenv0
    let (b, tenv2) = freshTv tenv1
    let (e, tenv3) = freshTv tenv2
    tenv4 <- unifyType tenv3 t ((a .-> b) e)
    Right (a, b, e, tenv4)

-- Type unification. There are two kinds of unification going on here: basic
-- logical unification for value types, and row unification for effect types.

unifyType :: TEnv -> Type -> Type -> Either String TEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> Right tenv0
  (TVar x, t) -> unifyTv tenv0 x t
  (_, TVar{}) -> commute
  (a, TForall x t) -> let
    (b, _, tenv1) = instantiate tenv0 x t
    in unifyType tenv1 a b
  (TForall{}, _) -> commute

-- Row unification is essentially unification of sets: we attempt to rewrite one
-- row into the other by swapping and dropping, and if we can do so then we know
-- they are isomorphic and unify successfully.
--
-- The occurs check prevents us from unifying rows with a common tail and a
-- distinct prefix, which could fail to terminate because the algorithm
-- generates fresh type variables.

  (TCon CJoin `TApp` l `TApp` r, s)
    | Just (TCon CJoin `TApp` _ `TApp` s', substitution, tenv1) <- rowIso tenv0 l s
    -> case substitution of
      Just (x, t)
        | occurs tenv0 x (effectTail r)
        -> Left $ unwords ["cannot unify effects", show t1, "and", show t2]
        | otherwise
        -> let
          tenv2 = tenv1 { envTvs = Map.insert x t (envTvs tenv1) }
          in unifyType tenv2 r s'
      Nothing -> unifyType tenv1 r s'

  (_, TCon CJoin `TApp` _ `TApp` _) -> commute

{-

  (TCon CFun `TApp` a `TApp` b `TApp` _, f) -> do
    (c, d, _, tenv1) <- unifyFun tenv0 f
    tenv2 <- unifyType tenv1 a c
    unifyType tenv2 b d

  (f, TCon CFun `TApp` c `TApp` d `TApp` _) -> do
    (a, b, _, tenv1) <- unifyFun tenv0 f
    tenv2 <- unifyType tenv1 a c
    unifyType tenv2 b d

-}

-- We fall back to regular unification for value type constructors. This makes
-- the somewhat iffy assumption that there is no higher-kinded polymorphism
-- going on between value type constructors and effect type constructors.

  (a `TApp` b, c `TApp` d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d

  _ -> Left $ unwords ["cannot unify types", show t1, "and", show t2]

-- Unification is commutative. If we fail to handle a case, this can result in
-- an infinite loop.

  where
  commute = unifyType tenv0 t2 t1

-- The row-isomorphism operation takes an effect label and an effect row, and
-- asserts that the row can be rewritten to begin with that label under some
-- substitution. It returns the substitution and the tail of the rewritten row.

rowIso :: TEnv -> Type -> Type -> Maybe (Type, Maybe (TypeId, Type), TEnv)

-- The "head" rule: a row which already begins with the label is trivially
-- rewritten by the identity substitution.

rowIso tenv0 lin rin@(TCon CJoin `TApp` l `TApp` _)
  | l == lin = Just (rin, Nothing, tenv0)

-- The "swap" rule: a row which contains the label somewhere within, can be
-- rewritten to place that label at the head.

rowIso tenv0 l (TCon CJoin `TApp` l' `TApp` r)
  | l /= l' = do
  (r', substitution, tenv1) <- rowIso tenv0 l r
  Just (l .| l' .| r', substitution, tenv1)

-- The "var" rule: no label is present, so we cannot test for equality, and must
-- return a fresh variable for the row tail.

rowIso tenv0 l (TVar a) = let
  (b, tenv1) = freshTv tenv0
  res = l .| b
  in Just (res, Just (a, res), tenv1)

-- In any other case, the rows are not isomorphic.

rowIso _ _ _ = Nothing

-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.

unifyTv :: TEnv -> TypeId -> Type -> Either String TEnv
unifyTv tenv0 x t = case t of
  TVar y | x == y -> Right tenv0
  TVar{} -> declare
  _ -> if occurs tenv0 x t then Left "occurs check" else declare
  where
  declare = case Map.lookup x (envTvs tenv0) of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> Right tenv0 { envTvs = Map.insert x t (envTvs tenv0) }

-- Kind variable unification is virtually identical to type variable
-- unification. This is currently missing an occurs check.

unifyKv :: TEnv -> KindId -> Kind -> Either String TEnv
unifyKv tenv0 x k = case k of
  KVar y | x == y -> Right tenv0
  KVar{} -> declare
  _ -> declare
  where
  declare = case Map.lookup x (envKvs tenv0) of
    Just k2 -> unifyKind tenv0 k k2
    Nothing -> Right tenv0 { envKvs = Map.insert x k (envKvs tenv0) }

-- We need to be able to count occurrences of a type variable in a type for two
-- reasons: to prevent infinite types (the "occurs check"), and to determine
-- whether a stack variable can be generalized to a higher rank.

occurrences :: TEnv -> TypeId -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TCon{} -> 0
    TVar y -> case Map.lookup y (envTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TConst{} -> 0
    TForall x' t'
      -> if x == x' then 0 else recur t'
    a `TApp` b -> recur a + recur b

occurs :: TEnv -> TypeId -> Type -> Bool
occurs tenv0 x t = occurrences tenv0 x t > 0

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
    TVar x -> case Map.lookup x (envTvs tenv0) of
      Just (TVar x') | x == x' -> t
      Just t' -> recur t'
      Nothing -> t
    TConst{} -> t
    TForall x t'
      -> TForall x . zonkType tenv0 { envTvs = Map.delete x (envTvs tenv0) } $ t'
    a `TApp` b -> recur a `TApp` recur b

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
freeTvs t = case t of
  TCon{} -> Set.empty
  TVar x -> Set.singleton x
  TConst{} -> Set.empty
  TForall x t' -> Set.delete x (freeTvs t')
  a `TApp` b -> freeTvs a `Set.union` freeTvs b

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
  (t', vars) = runWriter $ go TopLevel t
  in foldr TForall t' $ foldr delete (Set.toList (freeTvs t')) vars
  where
  go :: TypeLevel -> Type -> Writer [TypeId] Type
  go level t' = case t' of
    TCon CFun `TApp` a `TApp` b `TApp` _
      | level == NonTopLevel
      , TVar c <- bottommost a
      , TVar d <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [c]
        return $ TForall c t'
    TForall{} -> error "cannot regeneralize higher-ranked type"
    a `TApp` b -> TApp <$> go NonTopLevel a <*> go NonTopLevel b
    _ -> return t'

data TypeLevel = TopLevel | NonTopLevel deriving (Eq)

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

instantiate :: TEnv -> TypeId -> Type -> (Type, Type, TEnv)
instantiate tenv0 x t = let
  (a, tenv1) = freshTv tenv0
  in (replaceTv x a t, a, tenv1)

instantiatePrenex :: TEnv -> Type -> (Type, [Type], TEnv)
instantiatePrenex tenv0 (TForall x t) = let
  (t', a, tenv1) = instantiate tenv0 x t
  (t'', as, tenv2) = instantiatePrenex tenv1 t'
  in (t'', a : as, tenv2)
instantiatePrenex tenv0 t = (t, [], tenv0)

-- To replace a type variable with a type throughout a type, we simply zonk in a
-- singleton typing environment.

replaceTv :: TypeId -> Type -> Type -> Type
replaceTv x a = zonkType emptyTEnv { envTvs = Map.singleton x a }

-- Skolemization replaces quantified type variables with type constants.

skolemize :: TEnv -> Type -> (Set TypeId, Type, TEnv)
skolemize tenv0 t = case t of
  TForall x t' -> let
    (k, tenv1) = freshTypeId tenv0
    tenv2 = tenv1 { envTvs = Map.insert x (TConst k) (envTvs tenv1) }
    (k', t'', tenv3) = skolemize tenv2 (zonkType tenv2 t')
    in (Set.insert k k', t'', tenv3)
  -- TForall _ t' -> skolemize tenv0 t'
  TCon CFun `TApp` a `TApp` b `TApp` e -> let
    (ids, b', tenv1) = skolemize tenv0 b
    in (ids, (a .-> b') e, tenv1)
  _ -> (Set.empty, t, tenv0)

-- Since skolem constants only unify with type variables and themselves,
-- unifying a skolemized scheme with a type tells you whether one is a generic
-- instance of the other. This is used to check the signatures of definitions.

instanceCheck :: Type -> Type -> Either String ()
instanceCheck inferredScheme declaredScheme = do
  let (inferredType, _, tenv0) = instantiatePrenex emptyTEnv inferredScheme
  let (ids, declaredType, tenv1) = skolemize tenv0 declaredScheme
  tenv2 <- case subsumptionCheck tenv1 inferredType declaredType of
    Left message -> Left (prefix ++ " (instantiated to " ++ show inferredType ++ " and " ++ show declaredType ++ ")" ++ ": " ++ message)
    Right tenv' -> Right tenv'
  let escaped = freeTvs inferredScheme `Set.union` freeTvs declaredScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) (Left prefix)
  Right ()
  where
  prefix = unwords [show inferredScheme, "is not an instance of", show declaredScheme]

subsumptionCheck :: TEnv -> Type -> Type -> Either String TEnv

subsumptionCheck tenv0 (TForall x t) t2 = do
  let (t1, _, tenv1) = instantiate tenv0 x t
  subsumptionCheck tenv1 t1 t2

subsumptionCheck tenv0 t1 (TCon CFun `TApp` a `TApp` b `TApp` e) = do
  (a', b', e', tenv1) <- unifyFun tenv0 t1
  subsumptionCheckFun tenv1 a b e a' b' e'

subsumptionCheck tenv0 (TCon CFun `TApp` a' `TApp` b' `TApp` e') t2 = do
  (a, b, e, tenv1) <- unifyFun tenv0 t2
  subsumptionCheckFun tenv1 a b e a' b' e'

subsumptionCheck tenv0 t1 t2 = unifyType tenv0 t1 t2

-- This handles contravariance.

subsumptionCheckFun :: TEnv -> Type -> Type -> Type -> Type -> Type -> Type -> Either String TEnv
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
    ECat tref a b -> showTyped tref $ unwords [show a, show b]
    EId tref -> showTyped tref $ ""
    EQuote tref expr -> showTyped tref $ "[" ++ show expr ++ "]"
    EGo tref name -> showTyped tref $ '&' : Text.unpack name
    ECome Move tref name -> showTyped tref $ '-' : Text.unpack name
    ECome Copy tref name -> showTyped tref $ '+' : Text.unpack name
    where
    showTyped (Just type_) x = "(" ++ x ++ " : " ++ show type_ ++ ")"
    showTyped Nothing x = x

{-
instance Show Type where
  showsPrec p t = case t of
    TCon con -> shows con
    TVar x -> shows x
    TConst x -> shows x
    TForall x t' -> showParen True $ showChar '\x2200' . shows (TVar x) . showString ". " . shows t'
    a `TApp` b -> showParen (p > appPrec) $ showsPrec appPrec a . showChar ' ' . showsPrec (appPrec + 1) b
    where
    appPrec = 1
-}

instance Show TypeId where
  show (TypeId x) = 't' : show x

instance Show Con where
  show con = case con of
    CInt -> "int"
    CProd -> "pair"
    CFun -> "fun"
    CPure -> "pure"
    CIO -> "io"
    CFail -> "fail"
    CJoin -> "join"

instance Show Kind where
  showsPrec p k = case k of
    KVal -> showString "val"
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
      map (\ (t, t') -> show (TVar t) ++ " ~ " ++ show t') (Map.toList (envTvs tenv)),
      map (\ (k, k') -> show (KVar k) ++ " ~ " ++ show k') (Map.toList (envKvs tenv)),
      map (\ (t, k) -> show (TVar t) ++ " : " ++ show k) (Map.toList (envTks tenv)),
      map (\ (v, t) -> Text.unpack v ++ " : " ++ show t) (Map.toList (envVs tenv)) ],
    " }" ]

-- Utility operators for constructing types and kinds.

(.->) :: Type -> Type -> Type -> Type
(t1 .-> t2) e = cfun `TApp` t1 `TApp` t2 `TApp` e
infixr 4 .->

(.*) :: Type -> Type -> Type
t1 .* t2 = cprod `TApp` t1 `TApp` t2
infixl 5 .*

(.|) :: Type -> Type -> Type
t1 .| t2 = cjoin `TApp` t1 `TApp` t2
infixr 4 .|

(..->) :: Kind -> Kind -> Kind
(..->) = KFun
infixr 4 ..->

-- Generating names and named things from a typing environment.

freshTv :: TEnv -> (Type, TEnv)
freshTv = first TVar . freshTypeId

freshTc :: TEnv -> (Type, TEnv)
freshTc = first TConst . freshTypeId

freshKv :: TEnv -> (Kind, TEnv)
freshKv = first KVar . freshKindId

freshTypeId :: TEnv -> (TypeId, TEnv)
freshTypeId tenv = (envCurrentType tenv, tenv { envCurrentType = succ (envCurrentType tenv) })

freshKindId :: TEnv -> (KindId, TEnv)
freshKindId tenv = (envCurrentKind tenv, tenv { envCurrentKind = succ (envCurrentKind tenv) })

-- Parses an expression, for interactive testing purposes.

parse :: String -> Expr
parse = foldl' (ECat Nothing) (EId Nothing) . map toExpr . words
  where
  toExpr s = if all isDigit s
    then EPush Nothing . VInt . read $ s
    else case s of
      '.' : ss -> EQuote Nothing $ ECall Nothing (Text.pack ss) []
      '&' : ss -> EGo Nothing . Text.pack $ ss
      '+' : ss -> ECome Copy Nothing . Text.pack $ ss
      '-' : ss -> ECome Move Nothing . Text.pack $ ss
      _ -> ECall Nothing (Text.pack s) []
