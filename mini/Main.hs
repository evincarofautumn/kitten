{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Debug.Trace

main :: IO ()
main = return ()

type Tc a = State TEnv a

data Expr
  = EPush Val
  | ECall Name
  | ECat Expr Expr
  | EQuote Expr
  | EId
  deriving (Eq)

instance Show Expr where
  show e = case e of
    EPush val -> show val
    ECall name -> Text.unpack name
    ECat a b -> unwords [show a, show b]
    EId -> ""
    EQuote expr -> '.' : show expr

data Type
  = TInt
  | TVar (Id Type)
  | TFun Type Type
  | TProd Type Type
  | TQuantified Scheme
  deriving (Eq)

(.->) :: Type -> Type -> Type
(.->) = TFun
infixr 4 .->

(.*) :: Type -> Type -> Type
(.*) = TProd
infixl 5 .*

instance Show Type where
  showsPrec p t = case t of
    TInt -> showString "int"
    TVar x -> shows x
    a `TFun` b -> showParen (p > funPrec) $ showsPrec (funPrec + 1) a . showString " \x2192 " . showsPrec funPrec b
    a `TProd` b -> showParen (p > prodPrec) $ showsPrec prodPrec a . showString " \xD7 " . showsPrec (prodPrec + 1) b
    TQuantified scheme -> showParen True . shows $ scheme
    where
    prodPrec = 2
    funPrec = 1

data Scheme
  = Forall (Set (Id Type)) Type
  deriving (Eq)

instance Show Scheme where
  show (Forall ids t) = '\x2200' : (unwords . map (show . TVar) . Set.toList) ids ++ ". " ++ show t

data Kind
  = KStar
  | KRho
  | KFun Kind Kind
  | KVar (Id Kind)
  deriving (Eq)

(..->) :: Kind -> Kind -> Kind
(..->) = KFun
infixr 4 ..->

instance Show Kind where
  showsPrec p k = case k of
    KStar -> showString "*"
    KRho -> showString "\x03C1"
    a `KFun` b -> showParen (p > funPrec) $ showsPrec (funPrec + 1) a . showString " \x2192 " . showsPrec funPrec b
    KVar x -> shows x
    where
    funPrec = 1

data Val = VInt Int deriving (Eq)

instance Show Val where
  show (VInt i) = show i

newtype Id a = Id { unId :: Int }
  deriving (Enum, Eq, Ord)

instance Show (Id Type) where 
  show (Id x) = 't' : show x

instance Show (Id Kind) where
  show (Id x) = 'k' : show x

type Name = Text

data TEnv = TEnv {
  envTvs :: Map (Id Type) Type,  -- What is this type variable equal to?
  envTks :: Map (Id Type) Kind,  -- What kind does this type variable have?
  envKvs :: Map (Id Kind) Kind,  -- What is this kind variable equal to?
  envCurrentType :: Id Type,
  envCurrentKind :: Id Kind }

instance Show TEnv where
  show tenv = concat [
    "{ ",
    intercalate ", " $ concat [
      map (\ (t, t') -> show (TVar t) ++ " ~ " ++ show t') (Map.toList (envTvs tenv)),
      map (\ (k, k') -> show (KVar k) ++ " ~ " ++ show k') (Map.toList (envKvs tenv)),
      map (\ (t, k) -> show (TVar t) ++ " : " ++ show k) (Map.toList (envTks tenv)) ],
    " }" ]

inferType0 :: Expr -> (Scheme, Kind)
inferType0 expr = let
  (t, tenv1) = inferType emptyTEnv expr
  zonked = zonkType tenv1 t
  (kind, tenv2) = inferKind tenv1 zonked
  (Forall _ids demoted, tenv3) = demote tenv2 zonked
  regeneralized = regeneralize tenv3 demoted
  in trace (show tenv3 ++ " \x22A2 " ++ show expr ++ " : " ++ show regeneralized ++ " : " ++ show kind) (regeneralized, kind)

defaultKinds :: TEnv -> Kind -> TEnv
defaultKinds tenv0 = foldr (\ x tenv -> unifyKind tenv (KVar x) KStar) tenv0 . Set.toList . freeKvs

inferKind :: TEnv -> Type -> (Kind, TEnv)
inferKind tenv0 t = case t of
  TInt -> (KStar, tenv0)
  TVar x -> case Map.lookup x (envTks tenv0) of
    Just k' -> (k', tenv0)
    Nothing -> let
      (k', tenv1) = freshKv tenv0
      in (k', tenv1 { envTks = Map.insert x k' (envTks tenv1) })
  t1 `TFun` t2 -> let
    (k1, tenv1) = inferKind tenv0 t1
    (k2, tenv2) = inferKind tenv1 t2
    tenv3 = unifyKind tenv2 k1 KRho
    tenv4 = unifyKind tenv3 k2 KRho
    in (KStar, tenv4)
  t1 `TProd` t2 -> let
    (k1, tenv1) = inferKind tenv0 t1
    (k2, tenv2) = inferKind tenv1 t2
    tenv3 = unifyKind tenv2 k1 KRho
    tenv4 = unifyKind tenv3 k2 KStar
    in (KRho, tenv4)
  TQuantified (Forall tvs t') -> let
    (k1, _) = inferKind (foldr (\ x tenv -> let (a, tenv') = freshKv tenv in tenv' { envTks = Map.insert x a (envTks tenv') }) tenv0 . Set.toList $ tvs) t'
    tenv1 = unifyKind tenv0 k1 KStar
    in (k1, tenv1)

unifyKind :: TEnv -> Kind -> Kind -> TEnv
unifyKind tenv0 k1 k2 = case (k1, k2) of
  _ | k1 == k2 -> tenv0
  (KVar x, t) -> unifyKv tenv0 x t
  (_, KVar{}) -> commute
  (a `KFun` b, c `KFun` d) -> let
    tenv1 = unifyKind tenv0 a c
    in unifyKind tenv1 b d
  _ -> error $ unwords ["cannot unify kinds", show k1, "and", show k2]
  where
  commute = unifyKind tenv0 k2 k1

emptyTEnv :: TEnv
emptyTEnv = TEnv {
  envTvs = Map.empty,
  envTks = Map.empty,
  envKvs = Map.empty,
  envCurrentType = Id 0,
  envCurrentKind = Id 0 }

inferType :: TEnv -> Expr -> (Type, TEnv)
inferType tenv0 expr = case expr of
  EPush val -> let
    (t, tenv1) = inferVal tenv0 val
    (a, tenv2) = freshTv tenv1
    in (a .-> a .* t, tenv2)
  ECall "add" -> let
    (a, tenv1) = freshTv tenv0
    in (a .* TInt .* TInt .-> a .* TInt, tenv1)
  {-
  -- Should cause a kind mismatch (* ~ ρ) if used in a program.
  ECall "snd" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    in (a .* b .-> b, tenv2)
  -}
  ECall "cat" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (c, tenv3) = freshTv tenv2
    (d, tenv4) = freshTv tenv3
    in (a .* (b .-> c) .* (c .-> d) .-> a .* (b .-> d), tenv4)
  ECall "app" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    in (a .* (a .-> b) .-> b, tenv2)
  ECall "quo" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (ci, tenv3) = freshTypeId tenv2
    c = TVar ci
    in (a .* b .-> a .* TQuantified (Forall (Set.singleton ci) (c .-> c .* b)), tenv3)
  ECall name -> error $ "cannot infer type of " ++ show name
  expr1 `ECat` expr2 -> let
    (t1, tenv1) = inferType tenv0 expr1
    (t2, tenv2) = inferType tenv1 expr2
    (a, b, tenv3) = unifyFun tenv2 t1
    (c, d, tenv4) = unifyFun tenv3 t2
    tenv5 = unifyType tenv4 b c
    in (a .-> d, tenv5)
  EId -> let
    (a, tenv1) = freshTv tenv0
    in (a .-> a, tenv1)
  EQuote e -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = inferType tenv1 e
    in (a .-> a .* b, tenv2)

freshTv :: TEnv -> (Type, TEnv)
freshTv = first TVar . freshTypeId

freshKv :: TEnv -> (Kind, TEnv)
freshKv = first KVar . freshKindId

freshTypeId :: TEnv -> (Id Type, TEnv)
freshTypeId tenv = (envCurrentType tenv, tenv { envCurrentType = succ (envCurrentType tenv) })

freshKindId :: TEnv -> (Id Kind, TEnv)
freshKindId tenv = (envCurrentKind tenv, tenv { envCurrentKind = succ (envCurrentKind tenv) })

unifyType :: TEnv -> Type -> Type -> TEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> tenv0
  (TVar x, t) -> unifyTv tenv0 x t
  (_, TVar{}) -> commute
  (a `TFun` b, c `TFun` d) -> let
    tenv1 = unifyType tenv0 a c
    in unifyType tenv1 b d
  (a `TProd` b, c `TProd` d) -> let
    tenv1 = unifyType tenv0 a c
    in unifyType tenv1 b d
  (a, TQuantified scheme) -> let
    (b, tenv1) = instantiate tenv0 scheme
    in unifyType tenv1 a b
  (TQuantified{}, _) -> commute
  _ -> error $ unwords ["cannot unify types", show t1, "and", show t2]
  where
  commute = unifyType tenv0 t2 t1

unifyTv :: TEnv -> Id Type -> Type -> TEnv
unifyTv tenv0 x t = case t of
  TVar y | x == y -> tenv0
  TVar{} -> declare
  _ -> if occurs tenv0 x t then error "occurs check" else declare
  where
  declare = case Map.lookup x (envTvs tenv0) of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> tenv0 { envTvs = Map.insert x t (envTvs tenv0) }

unifyKv :: TEnv -> Id Kind -> Kind -> TEnv
unifyKv tenv0 x k = case k of
  KVar y | x == y -> tenv0
  KVar{} -> declare
  -- TODO: occurs check?
  _ -> declare
  where
  declare = case Map.lookup x (envKvs tenv0) of
    Just k2 -> unifyKind tenv0 k k2
    Nothing -> tenv0 { envKvs = Map.insert x k (envKvs tenv0) }

occurs :: TEnv -> Id Type -> Type -> Bool
occurs = (> 0) ... occurrences

(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) = (.) . (.) . (.)

occurrences :: TEnv -> Id Type -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TInt -> 0
    TVar y -> case Map.lookup y (envTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    a `TFun` b -> recur a + recur b
    a `TProd` b -> recur a + recur b
    TQuantified (Forall tvs t')
      -> if Set.member x tvs then 0 else recur t'

inferVal :: TEnv -> Val -> (Type, TEnv)
inferVal tenv val = case val of
  VInt{} -> (TInt, tenv)

unifyFun :: TEnv -> Type -> (Type, Type, TEnv)
unifyFun tenv0 t = case t of
  a `TFun` b -> (a, b, tenv0)
  _ -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    tenv3 = unifyType tenv2 t (a .-> b)
    in (a, b, tenv3)

zonkType :: TEnv -> Modify Type
zonkType tenv0 = recur
  where
  recur t = case t of
    TInt -> t
    TVar x -> case Map.lookup x (envTvs tenv0) of
      Just t' -> recur t'
      Nothing -> t
    a `TFun` b -> recur a .-> recur b
    a `TProd` b -> recur a .* recur b
    TQuantified (Forall tvs t')
      -> TQuantified . Forall tvs . zonkType tenv0 { envTvs = foldr Map.delete (envTvs tenv0) . Set.toList $ tvs } $ t'

zonkKind :: TEnv -> Modify Kind
zonkKind tenv0 = recur
  where
  recur k = case k of
    KStar -> k
    KRho -> k
    KVar x -> case Map.lookup x (envKvs tenv0) of
      Just k' -> recur k'
      Nothing -> k
    a `KFun` b -> recur a ..-> recur b

parse :: String -> Expr
parse = foldl' ECat EId . map toExpr . words
  where
  toExpr s = if all isDigit s
    then EPush . VInt . read $ s
    else case s of
      '.' : ss -> EQuote . ECall . Text.pack $ ss
      _ -> ECall . Text.pack $ s

freeTvs :: Type -> Set (Id Type)
freeTvs t = case t of
  TInt -> Set.empty
  TVar x -> Set.singleton x
  a `TFun` b -> freeTvs a `Set.union` freeTvs b
  a `TProd` b -> freeTvs a `Set.union` freeTvs b
  TQuantified (Forall ids t') -> freeTvs t' Set.\\ ids

freeKvs :: Kind -> Set (Id Kind)
freeKvs k = case k of
  KStar -> Set.empty
  KRho -> Set.empty
  a `KFun` b -> freeKvs a `Set.union` freeKvs b
  KVar x -> Set.singleton x

data TypeLevel = TopLevel | NonTopLevel deriving (Eq)

regeneralize :: TEnv -> Type -> Scheme
regeneralize tenv t = let
  (t', vars) = runWriter $ go TopLevel t
  in Forall (foldr Set.delete (freeTvs t') vars) t'
  where
  go :: TypeLevel -> Type -> Writer [Id Type] Type
  go level t' = case t' of
    a `TFun` b
      | level == NonTopLevel
      , TVar c <- bottommost a
      , TVar d <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [c]
        return . TQuantified . Forall (Set.singleton c) $ t'
    a `TFun` b -> TFun <$> go NonTopLevel a <*> go NonTopLevel b
    a `TProd` b -> TProd <$> go NonTopLevel a <*> go NonTopLevel b
    -- I don't think this is correct.
    TQuantified (Forall _ t'') -> pure . TQuantified $ regeneralize tenv t''
    _ -> return t'

bottommost :: Type -> Type
bottommost (a `TProd` _) = bottommost a
bottommost t = t

instantiate :: TEnv -> Scheme -> (Type, TEnv)
instantiate tenv0 (Forall ids t) = let
  (t', tenv1) = foldr go (t, emptyTEnv) . Set.toList $ ids
  in (t', tenv1 { envTvs = envTvs tenv0 `Map.union` envTvs tenv1 })
  where
  go x (t', tenv) = let
    (a, tenv') = freshTv tenv
    in (replaceTv x a t', tenv')

replaceTv :: Id Type -> Type -> Type -> Type
replaceTv x a = zonkType emptyTEnv { envTvs = Map.singleton x a }

-- Reduces the rank of rho-kinded type variables.
demote :: TEnv -> Type -> (Scheme, TEnv)
demote tenv0 t = let
  (t', ids, tenv1) = demote' tenv0 t
  in (Forall ids t', tenv1)

demote' :: TEnv -> Type -> (Type, Set (Id Type), TEnv)
demote' tenv0 t = case t of
  TInt -> (t, Set.empty, tenv0)
  TVar{} -> (t, Set.empty, tenv0)
  a `TFun` b -> let
    (a', ids1, tenv1) = demote' tenv0 a
    (b', ids2, tenv2) = demote' tenv1 b
    in (a' `TFun` b', ids1 `Set.union` ids2, tenv2)
  a `TProd` b -> let
    (a', ids1, tenv1) = demote' tenv0 a
    (b', ids2, tenv2) = demote' tenv1 b
    in (a' `TProd` b', ids1 `Set.union` ids2, tenv2)
  TQuantified (Forall ids t') -> let
    (t'', ids', tenv') = foldr go (t', Set.empty, tenv0) . Set.toList $ ids
    in (TQuantified (Forall (ids Set.\\ ids') t''), ids', tenv')
    where
    go x (t'', ids', tenv) = case Map.lookup x (envTks tenv) of
      Just KRho -> let
        (a, tenv') = freshTv tenv
        t''' = replaceTv x a t''
        in (t''', Set.insert x ids', tenv')
      _ -> (t', ids', tenv)

type Modify a = a -> a
