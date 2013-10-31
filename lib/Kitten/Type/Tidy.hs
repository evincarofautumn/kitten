{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Kitten.Type.Tidy
  ( Tidy
  , TidyType(..)
  , runTidy
  , tidyEffect
  , tidyEffectType
  , tidyRow
  , tidyRowType
  , tidyScalar
  , tidyScalarType
  ) where

import Control.Applicative
import Control.Monad.Trans.State

import Kitten.Name
import Kitten.NameMap (NameMap)
import Kitten.Type

import qualified Kitten.NameMap as NameMap
import qualified Kitten.Util.Set as Set

data TidyKindState a = TidyKindState
  { names :: !(NameMap (TypeName a))
  , nameGen :: !NameGen
  }

data TidyState = TidyState
  { scalars :: !(TidyKindState Scalar)
  , rows :: !(TidyKindState Row)
  , effects :: !(TidyKindState Effect)
  }

newtype Tidy a = Tidy (State TidyState a)
  deriving (Applicative, Functor, Monad)

class TidyType (a :: Kind) where
  tidyType :: Type a -> Tidy (Type a)

instance TidyType Scalar where
  tidyType = tidyScalarType

instance TidyType Row where
  tidyType = tidyRowType

instance TidyType Effect where
  tidyType = tidyEffectType

emptyKindState :: TidyKindState a
emptyKindState = TidyKindState
  { names = NameMap.empty
  , nameGen = mkNameGenFrom (Name 1)
  }

runTidy :: Tidy a -> a
runTidy (Tidy m) = evalState m TidyState
  { scalars = emptyKindState
  , rows = emptyKindState
  , effects = emptyKindState
  }

tidyName
  :: TypeName a
  -> TidyKindState a
  -> (TypeName a, TidyKindState a)
tidyName (TypeName name) kindState
  = case NameMap.lookup name (names kindState) of
    Just typeName -> (typeName, kindState)
    Nothing -> (TypeName name', kindState')
      where
      (name', gen') = genName (nameGen kindState)
      kindState' = kindState
        { names = NameMap.insert name
          (TypeName name') (names kindState)
        , nameGen = gen'
        }

tidyNameM
  :: (TidyState -> TidyKindState a)               -- ^ Get.
  -> (TidyKindState a -> TidyState -> TidyState)  -- ^ Put.
  -> TypeName a
  -> Tidy (TypeName a)
tidyNameM getKindState putKindState typeName = Tidy $ do
  kindState <- gets getKindState
  let (typeName', kindState') = tidyName typeName kindState
  modify (putKindState kindState')
  return typeName'

tidyScalar :: TypeName Scalar -> Tidy (TypeName Scalar)
tidyScalar = tidyNameM scalars
  (\scalars' s -> s { scalars = scalars' })

tidyRow :: TypeName Row -> Tidy (TypeName Row)
tidyRow = tidyNameM rows
  (\rows' s -> s { rows = rows' })

tidyEffect :: TypeName Effect -> Tidy (TypeName Effect)
tidyEffect = tidyNameM effects
  (\effects' s -> s { effects = effects' })

tidyScalarType :: Type Scalar -> Tidy (Type Scalar)
tidyScalarType type_ = case type_ of
  t1 :& t2 -> (:&) <$> tidyScalarType t1 <*> tidyScalarType t2
  (:?) t -> (:?) <$> tidyScalarType t
  t1 :| t2 -> (:|) <$> tidyScalarType t1 <*> tidyScalarType t2
  Bool{} -> pure type_
  Char{} -> pure type_
  Float{} -> pure type_
  Function r1 r2 e loc -> Function
    <$> tidyRowType r1
    <*> tidyRowType r2
    <*> tidyEffectType e
    <*> pure loc
  Handle{} -> pure type_
  Int{} -> pure type_
  Named{} -> pure type_
  Quantified (Forall r s e t) loc -> Quantified
    <$> (Forall
      <$> Set.mapM tidyRow r
      <*> Set.mapM tidyScalar s
      <*> Set.mapM tidyEffect e
      <*> tidyScalarType t)
    <*> pure loc
  Var name loc -> Var <$> tidyScalar name <*> pure loc
  Unit{} -> pure type_
  Vector t loc -> Vector <$> tidyScalarType t <*> pure loc

tidyRowType :: Type Row -> Tidy (Type Row)
tidyRowType type_ = case type_ of
  t1 :. t2 -> (:.) <$> tidyRowType t1 <*> tidyScalarType t2
  Empty{} -> pure type_
  Var name loc -> Var <$> tidyRow name <*> pure loc

tidyEffectType :: Type Effect -> Tidy (Type Effect)
tidyEffectType type_ = case type_ of
  t1 :+ t2 -> (:+) <$> tidyEffectType t1 <*> tidyEffectType t2
  Var name loc -> Var <$> tidyEffect name <*> pure loc
  NoEffect{} -> pure type_
  IOEffect{} -> pure type_
