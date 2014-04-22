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

import Kitten.Id
import Kitten.IdMap (IdMap)
import Kitten.Type

import qualified Kitten.IdMap as Id
import qualified Kitten.Util.Set as Set

data TidyKindState a = TidyKindState
  { ids :: !(IdMap (TypeId a))
  , idGen :: !IdGen
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
  :: TypeId a
  -> TidyKindState a
  -> (TypeId a, TidyKindState a)
tidyId (TypeId i) kindState
  = case Id.lookup i (ids kindState) of
    Just typeId -> (typeId, kindState)
    Nothing -> (TypeId i', kindState')
      where
      (i', gen') = genId (idGen kindState)
      kindState' = kindState
        { ids = Id.insert i
          (TypeId i') (ids kindState)
        , idGen = gen'
        }

tidyIdM
  :: (TidyState -> TidyKindState a)               -- ^ Get.
  -> (TidyKindState a -> TidyState -> TidyState)  -- ^ Put.
  -> TypeId a
  -> Tidy (TypeId a)
tidyIdM getKindState putKindState typeId = Tidy $ do
  kindState <- gets getKindState
  let (typeId', kindState') = tidyId typeId kindState
  modify (putKindState kindState')
  return typeId'

tidyScalar :: TypeId Scalar -> Tidy (TypeId Scalar)
tidyScalar = tidyIdM scalars
  (\scalars' s -> s { scalars = scalars' })

tidyStack :: TypeId Stack -> Tidy (TypeId Stack)
tidyStack = tidyIdM stacks
  (\stacks' s -> s { stacks = stacks' })

tidyScalarType :: Type Scalar -> Tidy (Type Scalar)
tidyScalarType type_ = case type_ of
  t1 :& t2 -> (:&) <$> tidyScalarType t1 <*> tidyScalarType t2
  (:?) t -> (:?) <$> tidyScalarType t
  t1 :| t2 -> (:|) <$> tidyScalarType t1 <*> tidyScalarType t2
  Const i loc -> Const <$> tidyScalar i <*> pure loc
  Ctor{} -> pure type_
  Function r1 r2 loc -> Function
    <$> tidyStackType r1
    <*> tidyStackType r2
    <*> pure loc
  Quantified (Forall r s t) loc -> Quantified
    <$> (Forall
      <$> Set.mapM tidyStack r
      <*> Set.mapM tidyScalar s
      <*> tidyScalarType t)
    <*> pure loc
  Var i loc -> Var <$> tidyScalar i <*> pure loc
  Vector t loc -> Vector <$> tidyScalarType t <*> pure loc

tidyStackType :: Type Stack -> Tidy (Type Stack)
tidyStackType type_ = case type_ of
  t1 :. t2 -> (:.) <$> tidyStackType t1 <*> tidyScalarType t2
  Const i loc -> Const <$> tidyStack i <*> pure loc
  Empty{} -> pure type_
  Var i loc -> Var <$> tidyStack i <*> pure loc
