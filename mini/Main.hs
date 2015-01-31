{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = return ()

type TRef = Maybe Type

untyped :: TRef
untyped = Nothing

data HowCome = Move | Copy
  deriving (Eq)

data Expr
  = EPush TRef Val
  | ECall TRef Name
  | ECat TRef Expr Expr
  | EQuote TRef Expr
  | EId TRef
  | EGo TRef Name
  | ECome HowCome TRef Name
  deriving (Eq)

instance Show Expr where
  show e = case e of
    EPush tref val -> showTyped tref $ show val
    ECall tref name -> showTyped tref $ Text.unpack name
    ECat tref a b -> showTyped tref $ unwords [show a, show b]
    EId tref -> showTyped tref $ ""
    EQuote tref expr -> showTyped tref $ "[" ++ show expr ++ "]"
    EGo tref name -> showTyped tref $ '&' : Text.unpack name
    ECome Move tref name -> showTyped tref $ '-' : Text.unpack name
    ECome Copy tref name -> showTyped tref $ '+' : Text.unpack name
    where
    showTyped (Just type_) x = "(" ++ x ++ " : " ++ show type_ ++ ")"
    showTyped Nothing x = x

data Type
  = TCon Con
  | TVar (Id Type)
  | TConst (Id Type)
  | TQuantified Scheme
  | TApp Type Type
  deriving (Eq)

data Con
  = CInt
  | CFun
  | CProd
  | CPure
  | CIO
  | CFail
  | CJoin
  deriving (Eq)

(.->) :: Type -> Type -> Type -> Type
(t1 .-> t2) e = TCon CFun `TApp` t1 `TApp` t2 `TApp` e
infixr 4 .->

(.*) :: Type -> Type -> Type
t1 .* t2 = TCon CProd `TApp` t1 `TApp` t2
infixl 5 .*

(.|) :: Type -> Type -> Type
t1 .| t2 = TCon CJoin `TApp` t1 `TApp` t2
infixr 4 .|

instance Show Type where
  showsPrec p t = case t of
    TCon con -> shows con
    TVar x -> shows x
    TConst x -> shows x
    TQuantified scheme -> showParen True . shows $ scheme
    a `TApp` b -> showParen (p > appPrec) $ showsPrec appPrec a . showChar ' ' . showsPrec (appPrec + 1) b
    where
    appPrec = 1

instance Show Con where
  show con = case con of
    CInt -> "int"
    CProd -> "pair"
    CFun -> "fun"
    CPure -> "pure"
    CIO -> "io"
    CFail -> "fail"
    CJoin -> "join"

data Scheme
  = Forall (Set (Id Type)) Type
  deriving (Eq)

instance Show Scheme where
  show (Forall ids t) = '\x2200' : (unwords . map (show . TVar) . Set.toList) ids ++ ". " ++ show t

data Kind
  = KVal
  | KRho
  | KEff
  | KFun Kind Kind
  | KVar (Id Kind)
  deriving (Eq)

(..->) :: Kind -> Kind -> Kind
(..->) = KFun
infixr 4 ..->

instance Show Kind where
  showsPrec p k = case k of
    KVal -> showString "val"
    KRho -> showString "\x03C1"
    KEff -> showString "\x03B5"
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
  envVs :: Map Name Type,  -- What type does this variable have?
  envSigs :: Map Name Scheme,  -- What type scheme does this word have?
  envCurrentType :: Id Type,
  envCurrentKind :: Id Kind }

instance Show TEnv where
  show tenv = concat [
    "{ ",
    intercalate ", " $ concat [
      map (\ (t, t') -> show (TVar t) ++ " ~ " ++ show t') (Map.toList (envTvs tenv)),
      map (\ (k, k') -> show (KVar k) ++ " ~ " ++ show k') (Map.toList (envKvs tenv)),
      map (\ (t, k) -> show (TVar t) ++ " : " ++ show k) (Map.toList (envTks tenv)),
      map (\ (v, t) -> Text.unpack v ++ " : " ++ show t) (Map.toList (envVs tenv)) ],
    " }" ]

inferTypes :: Map Name (Scheme, Expr) -> Either String (Map Name (Scheme, Expr))
inferTypes defs = fmap Map.fromList . mapM go . Map.toList $ defs
  where
  sigs = Map.map fst defs
  go (name, (scheme, expr)) = do
    (expr', scheme', _) <- inferType0 sigs expr
    instanceCheck scheme' scheme
    Right (name, (scheme, expr'))

inferType0 :: Map Name Scheme -> Expr -> Either String (Expr, Scheme, Kind)
inferType0 sigs expr = do
  (expr', t, tenv1) <- inferType emptyTEnv { envSigs = sigs } expr
  let zonkedType = zonkType tenv1 t
  let zonkedExpr = zonkExpr tenv1 expr'
  (kind, tenv2) <- inferKind tenv1 zonkedType
  let (Forall _ids demoted, tenv3) = demote tenv2 zonkedType
  let regeneralized = regeneralize tenv3 demoted
  tenv4 <- defaultKinds tenv3 kind
  let zonkedKind = zonkKind tenv4 kind
  Right (zonkedExpr, regeneralized, zonkedKind)

defaultKinds :: TEnv -> Kind -> Either String TEnv
defaultKinds tenv0 = foldrM (\ x tenv -> unifyKind tenv (KVar x) KVal) tenv0 . Set.toList . freeKvs

inferKind :: TEnv -> Type -> Either String (Kind, TEnv)
inferKind tenv0 t = case t of
  TCon con -> Right (case con of
    CInt -> KVal
    CFun -> KRho ..-> KRho ..-> KEff ..-> KVal
    CProd -> KRho ..-> KVal ..-> KRho
    CPure -> KEff
    CIO -> KEff
    CFail -> KEff
    CJoin -> KEff ..-> KEff ..-> KEff, tenv0)
  TVar x -> case Map.lookup x (envTks tenv0) of
    Just k' -> Right (k', tenv0)
    Nothing -> let
      (k', tenv1) = freshKv tenv0
      in Right (k', tenv1 { envTks = Map.insert x k' (envTks tenv1) })
  TConst{} -> Left "cannot infer kind of skolem constant"
  TQuantified (Forall tvs t') -> do
    (k1, _) <- inferKind (foldr (\ x tenv -> let (a, tenv') = freshKv tenv in tenv' { envTks = Map.insert x a (envTks tenv') }) tenv0 . Set.toList $ tvs) t'
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

emptyTEnv :: TEnv
emptyTEnv = TEnv {
  envTvs = Map.empty,
  envTks = Map.empty,
  envKvs = Map.empty,
  envVs = Map.empty,
  envSigs = Map.empty,
  envCurrentType = Id 0,
  envCurrentKind = Id 0 }

inferType :: TEnv -> Expr -> Either String (Expr, Type, TEnv)
inferType tenv0 expr0 = case expr0 of
  EPush Nothing val -> let
    (val', t, tenv1) = inferVal tenv0 val
    (a, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .-> a .* t) e
    in Right (EPush (Just type_) val', type_, tenv3)
  ECall Nothing "add" -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .* TCon CInt .* TCon CInt .-> a .* TCon CInt) e
    in Right (ECall (Just type_) "add", type_, tenv2)
  ECall Nothing "cat" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (c, tenv3) = freshTv tenv2
    (d, tenv4) = freshTv tenv3
    (e, tenv5) = freshTv tenv4
    type_ = (a .* (b .-> c) e .* (c .-> d) e .-> a .* (b .-> d) e) e
    in Right (ECall (Just type_) "cat", type_, tenv5)
  ECall Nothing "app" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .* (a .-> b) e .-> b) e
    in Right (ECall (Just type_) "app", type_, tenv3)
  ECall Nothing "quo" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (ci, tenv3) = freshTypeId tenv2
    c = TVar ci
    (e, tenv4) = freshTv tenv3
    type_ = (a .* b .-> a .* TQuantified (Forall (Set.singleton ci) ((c .-> c .* b) e))) e
    in Right (ECall (Just type_) "quo", type_, tenv4)
  ECall Nothing "say" -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .* TCon CInt .-> a) (TCon CIO .| e)
    in Right (ECall (Just type_) "say", type_, tenv2)
  ECall Nothing "abort" -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .-> b) (TCon CFail .| e)
    in Right (ECall (Just type_) "say", type_, tenv3)
  ECall Nothing name -> case Map.lookup name (envSigs tenv0) of
    Just scheme -> let
      (type_, tenv1) = instantiate tenv0 scheme
      in Right (ECall (Just type_) name, type_, tenv1)
    Nothing -> Left $ "cannot infer type of " ++ show name
  ECat Nothing expr1 expr2 -> do
    (expr1', t1, tenv1) <- inferType tenv0 expr1
    (expr2', t2, tenv2) <- inferType tenv1 expr2
    (a, b, e1, tenv3) <- unifyFun tenv2 t1
    (c, d, e2, tenv4) <- unifyFun tenv3 t2
    tenv5 <- unifyType tenv4 b c
    tenv6 <- unifyType tenv5 e1 e2
    let type_ = (a .-> d) e1
    Right (ECat (Just type_) expr1' expr2', type_, tenv6)
  EId Nothing -> let
    (a, tenv1) = freshTv tenv0
    (e, tenv2) = freshTv tenv1
    type_ = (a .-> a) e
    in Right (EId (Just type_), type_, tenv2)
  EQuote Nothing expr -> do
    let (a, tenv1) = freshTv tenv0
    (expr', b, tenv2) <- inferType tenv1 expr
    let (e, tenv3) = freshTv tenv2
    let type_ = (a .-> a .* b) e
    Right (EQuote (Just type_) expr', type_, tenv3)
  EGo Nothing name -> let
    (a, tenv1) = freshTv tenv0
    (b, tenv2) = freshTv tenv1
    (e, tenv3) = freshTv tenv2
    type_ = (a .* b .-> a) e
    in Right (EGo (Just type_) name, type_, tenv3 { envVs = Map.insert name b (envVs tenv3) })
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

freshTv :: TEnv -> (Type, TEnv)
freshTv = first TVar . freshTypeId

freshTc :: TEnv -> (Type, TEnv)
freshTc = first TConst . freshTypeId

freshKv :: TEnv -> (Kind, TEnv)
freshKv = first KVar . freshKindId

freshTypeId :: TEnv -> (Id Type, TEnv)
freshTypeId tenv = (envCurrentType tenv, tenv { envCurrentType = succ (envCurrentType tenv) })

freshKindId :: TEnv -> (Id Kind, TEnv)
freshKindId tenv = (envCurrentKind tenv, tenv { envCurrentKind = succ (envCurrentKind tenv) })

unifyType :: TEnv -> Type -> Type -> Either String TEnv
unifyType tenv0 t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> Right tenv0
  (TVar x, t) -> unifyTv tenv0 x t
  (_, TVar{}) -> commute
  (a, TQuantified scheme) -> let
    (b, tenv1) = instantiate tenv0 scheme
    in unifyType tenv1 a b
  (TQuantified{}, _) -> commute
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
  (a `TApp` b, c `TApp` d) -> do
    tenv1 <- unifyType tenv0 a c
    unifyType tenv1 b d
  _ -> Left $ unwords ["cannot unify types", show t1, "and", show t2]
  where
  commute = unifyType tenv0 t2 t1

-- Asserts that a row can be rewritten to begin with a given
-- label. Produces, if possible, a substitution under which
-- this is true, and the tail of the rewritten row.
rowIso :: TEnv -> Type -> Type -> Maybe (Type, Maybe (Id Type, Type), TEnv)

-- head
rowIso tenv0 lin rin@(TCon CJoin `TApp` l `TApp` _) | l == lin
  = return (rin, Nothing, tenv0)

-- swap
rowIso tenv0 l (TCon CJoin `TApp` l' `TApp` r) | l /= l' = do
  (r', substitution, tenv1) <- rowIso tenv0 l r
  Just (l .| l' .| r', substitution, tenv1)

-- row-var
rowIso tenv0 l (TVar a) = let
  (b, tenv1) = freshTv tenv0
  res = l .| b
  in Just (res, Just (a, res), tenv1)

rowIso _ _ _ = Nothing

unifyTv :: TEnv -> Id Type -> Type -> Either String TEnv
unifyTv tenv0 x t = case t of
  TVar y | x == y -> Right tenv0
  TVar{} -> declare
  _ -> if occurs tenv0 x t then Left "occurs check" else declare
  where
  declare = case Map.lookup x (envTvs tenv0) of
    Just t2 -> unifyType tenv0 t t2
    Nothing -> Right tenv0 { envTvs = Map.insert x t (envTvs tenv0) }

unifyKv :: TEnv -> Id Kind -> Kind -> Either String TEnv
unifyKv tenv0 x k = case k of
  KVar y | x == y -> Right tenv0
  KVar{} -> declare
  -- TODO: occurs check?
  _ -> declare
  where
  declare = case Map.lookup x (envKvs tenv0) of
    Just k2 -> unifyKind tenv0 k k2
    Nothing -> Right tenv0 { envKvs = Map.insert x k (envKvs tenv0) }

occurs :: TEnv -> Id Type -> Type -> Bool
occurs = (> 0) ... occurrences

(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) = (.) . (.) . (.)

occurrences :: TEnv -> Id Type -> Type -> Int
occurrences tenv0 x = recur
  where
  recur t = case t of
    TCon{} -> 0
    TVar y -> case Map.lookup y (envTvs tenv0) of
      Nothing -> if x == y then 1 else 0
      Just t' -> recur t'
    TConst{} -> 0
    TQuantified (Forall tvs t')
      -> if Set.member x tvs then 0 else recur t'
    a `TApp` b -> recur a + recur b

inferVal :: TEnv -> Val -> (Val, Type, TEnv)
inferVal tenv val = case val of
  VInt{} -> (val, TCon CInt, tenv)

unifyFun :: TEnv -> Type -> Either String (Type, Type, Type, TEnv)
unifyFun tenv0 t = case t of
  TCon CFun `TApp` a `TApp` b `TApp` e -> Right (a, b, e, tenv0)
  _ -> do
    let (a, tenv1) = freshTv tenv0
    let (b, tenv2) = freshTv tenv1
    let (e, tenv3) = freshTv tenv2
    tenv4 <- unifyType tenv3 t ((a .-> b) e)
    Right (a, b, e, tenv4)

zonkType :: TEnv -> Modify Type
zonkType tenv0 = recur
  where
  recur t = case t of
    TCon{} -> t
    TVar x -> case Map.lookup x (envTvs tenv0) of
      Just (TVar x') | x == x' -> t
      Just t' -> recur t'
      Nothing -> t
    TConst{} -> t
    TQuantified (Forall tvs t')
      -> TQuantified . Forall tvs . zonkType tenv0 { envTvs = foldr Map.delete (envTvs tenv0) . Set.toList $ tvs } $ t'
    a `TApp` b -> recur a `TApp` recur b

zonkExpr :: TEnv -> Modify Expr
zonkExpr tenv0 = recur
  where
  recur expr = case expr of
    EPush tref val -> EPush (zonkTRef tref) (zonkVal tenv0 val)
    ECall tref name -> ECall (zonkTRef tref) name
    ECat tref e1 e2 -> ECat (zonkTRef tref) (recur e1) (recur e2)
    EQuote tref e -> EQuote (zonkTRef tref) (recur e)
    EId tref -> EId (zonkTRef tref)
    EGo tref name -> EGo (zonkTRef tref) name
    ECome how tref name -> ECome how (zonkTRef tref) name
  zonkTRef = fmap (zonkType tenv0)

zonkVal :: TEnv -> Modify Val
zonkVal _tenv val@VInt{} = val

zonkKind :: TEnv -> Modify Kind
zonkKind tenv0 = recur
  where
  recur k = case k of
    KVal -> k
    KRho -> k
    KEff -> k
    KVar x -> case Map.lookup x (envKvs tenv0) of
      Just (KVar x') | x == x' -> k
      Just k' -> recur k'
      Nothing -> k
    a `KFun` b -> recur a ..-> recur b

parse :: String -> Expr
parse = foldl' (ECat untyped) (EId untyped) . map toExpr . words
  where
  toExpr s = if all isDigit s
    then EPush untyped . VInt . read $ s
    else case s of
      '.' : ss -> EQuote untyped . ECall untyped . Text.pack $ ss
      '&' : ss -> EGo untyped . Text.pack $ ss
      '+' : ss -> ECome Copy untyped . Text.pack $ ss
      '-' : ss -> ECome Move untyped . Text.pack $ ss
      _ -> ECall untyped . Text.pack $ s

freeTvs :: Type -> Set (Id Type)
freeTvs t = case t of
  TCon{} -> Set.empty
  TVar x -> Set.singleton x
  TConst{} -> Set.empty
  TQuantified (Forall ids t') -> freeTvs t' Set.\\ ids
  a `TApp` b -> freeTvs a `Set.union` freeTvs b

freeKvs :: Kind -> Set (Id Kind)
freeKvs k = case k of
  KVal -> Set.empty
  KRho -> Set.empty
  KEff -> Set.empty
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
    TCon CFun `TApp` a `TApp` b
      | level == NonTopLevel
      , TVar c <- bottommost a
      , TVar d <- bottommost b
      , c == d
      -> do
        when (occurrences tenv c t == 2) $ tell [c]
        return . TQuantified . Forall (Set.singleton c) $ t'
    -- I don't think this is correct.
    TQuantified (Forall _ t'') -> pure . TQuantified $ regeneralize tenv t''
    a `TApp` b -> TApp <$> go NonTopLevel a <*> go NonTopLevel b
    _ -> return t'

bottommost :: Type -> Type
bottommost (TCon CProd `TApp` a `TApp` _) = bottommost a
bottommost t = t

effectTail :: Type -> Type
effectTail (TCon CJoin `TApp` _ `TApp` a) = effectTail a
effectTail t = t

instantiate :: TEnv -> Scheme -> (Type, TEnv)
instantiate tenv0 (Forall ids t) = foldr go (t, tenv0) . Set.toList $ ids
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
  TCon{} -> (t, Set.empty, tenv0)
  TVar{} -> (t, Set.empty, tenv0)
  TConst{} -> (t, Set.empty, tenv0)
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
  a `TApp` b -> let
    (a', ids1, tenv1) = demote' tenv0 a
    (b', ids2, tenv2) = demote' tenv1 b
    in (a' `TApp` b', ids1 `Set.union` ids2, tenv2)

type Modify a = a -> a

skolemize :: Scheme -> (Set (Id Type), Type)
skolemize (Forall ids t) = let
  (consts, tenv0) = foldr
    (\ _index (acc, tenv) -> let (a, tenv') = freshTypeId tenv in (a : acc, tenv'))
    ([], emptyTEnv)
    [1 .. Set.size ids]
  tenv1 = foldr
    (\ (x, c) tenv -> tenv { envTvs = Map.insert x (TConst c) (envTvs tenv) })
    tenv0
    (zip (Set.toList ids) consts)
  in skolemizeType (zonkType tenv1 t)

skolemizeType :: Type -> (Set (Id Type), Type)
skolemizeType t = case t of
  TCon CFun `TApp` a `TApp` b `TApp` e -> let
    (ids, b') = skolemizeType b
    in (ids, (a .-> b') e)
  TQuantified scheme -> skolemize scheme
  _ -> (Set.empty, t)

instanceCheck :: Scheme -> Scheme -> Either String ()
instanceCheck inferredScheme declaredScheme = do
  let (inferredType, tenv0) = instantiate emptyTEnv inferredScheme
  let (ids, declaredType) = skolemize declaredScheme
  case unifyType tenv0 inferredType declaredType of
    Left message -> Left (prefix ++ ": " ++ message)
    Right{} -> Right ()
  let escaped = freeTvs (TQuantified inferredScheme) `Set.union` freeTvs (TQuantified declaredScheme)
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) (Left prefix)
  Right ()
  where
  prefix = unwords [show inferredScheme, "is not an instance of", show declaredScheme]
