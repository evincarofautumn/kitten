{-|
Module      : Kitten.TypeEnv
Description : Type inference environment
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.TypeEnv
  ( TypeEnv(..)
  , empty
  , freshTv
  , freshTypeId
  , getClosed
  , typeFromSignature
  ) where

import Data.List (find, foldl')
import Kitten.Informer (Informer(..))
import qualified Kitten.Report as Report
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import Kitten.DataConstructor (DataConstructor)
import Kitten.Kind (Kind)
import Kitten.Monad (K)
import Kitten.Name
import Kitten.Origin (Origin)
import Kitten.Type (Type(..), TypeId(..), Var(..))
import qualified Kitten.Type as Type
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Text.PrettyPrint as Pretty
import Control.Monad.Trans.State (StateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Kitten.Signature (Signature)
import qualified Kitten.Signature as Signature
import Data.Foldable (foldrM)
import qualified Kitten.Kind as Kind
import Kitten.Entry.Parameter (Parameter(Parameter))

-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What are the constructors and fields of this type?
--  • What is the type of this type variable?
--  • What is the type of this local variable?
--  • What are the types of the current closure?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TypeEnv = TypeEnv
  { constructors :: !(Map Qualified [DataConstructor])
  , tvs :: !(Map TypeId Type)
  , vs :: [Type]
  , closure :: [Type]
  , sigs :: !(Map Qualified Type)
  , currentType :: !(IORef TypeId)
  }

empty :: TypeEnv
empty = TypeEnv
  { constructors = Map.empty
  , tvs = Map.empty
  , vs = []
  , closure = []
  , sigs = Map.empty
  , currentType = currentTypeId
  }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

freshTv :: TypeEnv -> Origin -> Kind -> K Type
freshTv tenv origin k = TypeVar origin <$> (Var <$> freshTypeId tenv <*> pure k)

freshTypeId :: TypeEnv -> K TypeId
freshTypeId tenv = do
  x <- liftIO $ readIORef $ currentType tenv
  liftIO $ writeIORef (currentType tenv) $ succ x
  return x

instance Pretty TypeEnv where
  pPrint tenv = Pretty.vcat
    $ map (\ (v, t) -> Pretty.hsep [pPrint v, "~", pPrint t])
    $ Map.toList $ tvs tenv

getClosed :: TypeEnv -> Closed -> Type
getClosed tenv name = case name of
  ClosedLocal (LocalIndex index) -> vs tenv !! index
  ClosedClosure (ClosureIndex index) -> closure tenv !! index

-- | Desugars a parsed signature into an actual type. We resolve whether names
-- refer to quantified type variables or data definitions, and make stack
-- polymorphism explicit.

typeFromSignature :: TypeEnv -> Signature -> K Type
typeFromSignature tenv signature0 = do
  (type_, env) <- flip runStateT SignatureEnv
    { sigEnvAnonymous = []
    , sigEnvVars = Map.empty
    } $ go signature0
  let
    forallAnonymous = Forall (Signature.origin signature0)
    forallVar (var, o) = Forall o var
  return
    $ foldr forallAnonymous
      (foldr forallVar type_ $ Map.elems $ sigEnvVars env)
    $ sigEnvAnonymous env
  where

  go :: Signature -> StateT SignatureEnv K Type
  go signature = case signature of
    Signature.Application a b _ -> (:@) <$> go a <*> go b
    Signature.Bottom o -> return $ Type.bottom o
    Signature.Function as bs es o -> do
      r <- lift $ freshTypeId tenv
      let var = Var r Kind.Stack
      let typeVar = TypeVar o var
      es' <- mapM (fromVar o) es
      (me, es'') <- lift $ permissionVar o es'
      Forall o var <$> makeFunction o typeVar as typeVar bs es'' me
    Signature.Quantified vars a o -> do
      original <- get
      (envVars, vars') <- foldrM ((lift .) . declare)
        (sigEnvVars original, []) vars
      modify $ \ env -> env { sigEnvVars = envVars }
      a' <- go a
      let result = foldr (Forall o) a' vars'
      put original
      return result
      where

      declare
        :: Parameter
        -> (Map Unqualified (Var, Origin), [Var])
        -> K (Map Unqualified (Var, Origin), [Var])
      declare (Parameter varOrigin name kind) (envVars, freshVars) = do
        x <- freshTypeId tenv
        let var = Var x kind
        return (Map.insert name (var, varOrigin) envVars, var : freshVars)

    Signature.Variable name o -> fromVar o name
    Signature.StackFunction r as s bs es o -> do
      let var = fromVar o
      r' <- go r
      s' <- go s
      es' <- mapM var es
      (me, es'') <- lift $ permissionVar o es'
      makeFunction o r' as s' bs es'' me
    -- TODO: Verify that the type contains no free variables.
    Signature.Type type_ -> return type_

  permissionVar :: Origin -> [Type] -> K (Maybe Type, [Type])
  permissionVar o types = case splitFind isTypeVar types of
    Just (preceding, type_, following) -> case find isTypeVar following of
      Nothing -> return (Just type_, preceding ++ following)
      Just type' -> do
        report $ Report.MultiplePermissionVariables o type_ type'
        halt
    Nothing -> return (Nothing, types)
    where
    isTypeVar TypeVar{} = True
    isTypeVar _ = False

  fromVar :: Origin -> GeneralName -> StateT SignatureEnv K Type
  fromVar o (UnqualifiedName name) = do
    existing <- gets $ Map.lookup name . sigEnvVars
    case existing of
      Just (var, varOrigin) -> return $ TypeVar varOrigin var
      Nothing -> lift $ do
        report $ Report.CannotResolveType o $ UnqualifiedName name
        halt
  fromVar o (QualifiedName name)
    = return $ TypeConstructor o $ Type.Constructor name
  fromVar _ name = error
    $ "incorrectly resolved name in signature: " ++ show name

  makeFunction
    :: Origin
    -> Type -> [Signature] -> Type -> [Signature] -> [Type] -> Maybe Type
    -> StateT SignatureEnv K Type
  makeFunction o r as s bs es me = do
    as' <- mapM go as
    bs' <- mapM go bs
    e <- case me of
      Just e -> return e
      Nothing -> do
        ex <- lift $ freshTypeId tenv
        let var = Var ex Kind.Permission
        modify $ \ env -> env { sigEnvAnonymous = var : sigEnvAnonymous env }
        return $ TypeVar o var
    return $ Type.fun o (stack r as') (stack s bs')
      $ foldr (Type.join o) e es
    where

    stack :: Type -> [Type] -> Type
    stack = foldl' $ Type.prod o

splitFind :: (Eq a) => (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFind f = go []
  where
  go acc (x : xs)
    | f x = Just (reverse acc, x, xs)
    | otherwise = go (x : acc) xs
  go _ [] = Nothing

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: [Var]
  , sigEnvVars :: !(Map Unqualified (Var, Origin))
  }
