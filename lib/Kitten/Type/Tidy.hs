{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Kitten.Type.Tidy
  ( Tidy
  , TidyType(..)
  , runTidy
  , tidyScalar
  , tidyScalarType
  , tidyStack
  , tidyStackType
  ) where

import Control.Applicative hiding (Const)
import Control.Monad.Trans.State

import qualified Data.Vector as V

import Kitten.Id
import Kitten.IdMap (IdMap)
import Kitten.Types

import qualified Kitten.IdMap as Id
import qualified Kitten.Util.Set as Set

data TidyKindState a = TidyKindState
  { ids :: !(IdMap TypeSpace (KindedId a))
  , idGen :: !(IdGen TypeSpace)
  }

data TidyState = TidyState
  { scalars :: !(TidyKindState Scalar)
  , stacks :: !(TidyKindState Stack)
  }

newtype Tidy a = Tidy (State TidyState a)
  deriving (Applicative, Functor, Monad)

class TidyType (a :: Kind) where
  tidyType :: Type a -> Tidy (Type a)

instance TidyType Scalar where
  tidyType = tidyScalarType

instance TidyType Stack where
  tidyType = tidyStackType

emptyKindState :: TidyKindState a
emptyKindState = TidyKindState
  { ids = Id.empty
  , idGen = mkIdGenFrom (Id 1)
  }

runTidy :: Tidy a -> a
runTidy (Tidy m) = evalState m TidyState
  { scalars = emptyKindState
  , stacks = emptyKindState
  }

tidyId
  :: KindedId a
  -> TidyKindState a
  -> (KindedId a, TidyKindState a)
tidyId (KindedId i) kindState
  = case Id.lookup i (ids kindState) of
    Just typeId -> (typeId, kindState)
    Nothing -> (KindedId i', kindState')
      where
      (i', gen') = genId (idGen kindState)
      kindState' = kindState
        { ids = Id.insert i
          (KindedId i') (ids kindState)
        , idGen = gen'
        }

tidyIdM
  :: (TidyState -> TidyKindState a)               -- ^ Get.
  -> (TidyKindState a -> TidyState -> TidyState)  -- ^ Put.
  -> KindedId a
  -> Tidy (KindedId a)
tidyIdM getKindState putKindState typeId = Tidy $ do
  kindState <- gets getKindState
  let (typeId', kindState') = tidyId typeId kindState
  modify (putKindState kindState')
  return typeId'

tidyScalar :: KindedId Scalar -> Tidy (KindedId Scalar)
tidyScalar = tidyIdM scalars
  (\scalars' s -> s { scalars = scalars' })

tidyStack :: KindedId Stack -> Tidy (KindedId Stack)
tidyStack = tidyIdM stacks
  (\stacks' s -> s { stacks = stacks' })

tidyScalarType :: Type Scalar -> Tidy (Type Scalar)
tidyScalarType type_ = case type_ of
  t1 :& t2 -> (:&) <$> tidyScalarType t1 <*> tidyScalarType t2
  (:?) t -> (:?) <$> tidyScalarType t
  t1 :@ ts -> (:@) <$> tidyScalarType t1 <*> V.mapM tidyScalarType ts
  t1 :| t2 -> (:|) <$> tidyScalarType t1 <*> tidyScalarType t2
  TyConst i loc -> TyConst <$> tidyScalar i <*> pure loc
  TyCtor{} -> pure type_
  TyFunction r1 r2 loc -> TyFunction
    <$> tidyStackType r1
    <*> tidyStackType r2
    <*> pure loc
  TyQuantified (Forall r s t) loc -> TyQuantified
    <$> (Forall
      <$> Set.mapM tidyStack r
      <*> Set.mapM tidyScalar s
      <*> tidyScalarType t)
    <*> pure loc
  TyVar i loc -> TyVar <$> tidyScalar i <*> pure loc
  TyVector t loc -> TyVector <$> tidyScalarType t <*> pure loc

tidyStackType :: Type Stack -> Tidy (Type Stack)
tidyStackType type_ = case type_ of
  t1 :. t2 -> (:.) <$> tidyStackType t1 <*> tidyScalarType t2
  TyConst i loc -> TyConst <$> tidyStack i <*> pure loc
  TyEmpty{} -> pure type_
  TyVar i loc -> TyVar <$> tidyStack i <*> pure loc
