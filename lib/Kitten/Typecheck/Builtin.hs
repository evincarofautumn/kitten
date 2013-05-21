{-# LANGUAGE GADTs #-}

module Kitten.Typecheck.Builtin
  ( typecheckBuiltin
  ) where

import Control.Applicative
import Control.Monad

import Kitten.Builtin (Builtin)
import Kitten.Type
import Kitten.Typecheck.Monad

import qualified Kitten.Builtin as Builtin

typecheckBuiltin :: Builtin -> Typecheck Builtin
typecheckBuiltin builtin = builtin <$ case builtin of
  Builtin.AddFloat -> binary FloatType
  Builtin.AddInt -> binary IntType

  Builtin.AndBool -> relational BoolType

  Builtin.AndInt -> binary IntType

  Builtin.Apply -> do
    Composition consumption :> Composition production
      <- popDataExpecting $ AnyType :> AnyType
    mapM_ popDataExpecting_ $ reverse consumption
    mapM_ pushData production

  Builtin.Bottom -> do
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.CatVector -> do
    VectorType b <- popDataExpecting $ VectorType AnyType
    VectorType a <- popDataExpecting $ VectorType AnyType
    if a == b
      then pushData $ VectorType a
      else mismatchedElements a b

  Builtin.Close -> popDataExpecting_ HandleType

  Builtin.Compose -> do
    Composition c :> Composition d
      <- popDataExpecting $ AnyType :> AnyType
    Composition a :> Composition b
      <- popDataExpecting $ AnyType :> AnyType
    result <- stackEffect $ do
      mapM_ popDataExpecting a
      mapM_ pushData b
      mapM_ popDataExpecting c
      mapM_ pushData d
    pushData result

  Builtin.DecFloat -> unary FloatType
  Builtin.DecInt -> unary IntType

  Builtin.DivFloat -> binary FloatType
  Builtin.DivInt -> binary IntType

  Builtin.Down -> do
    a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.EqChar -> relational CharType
  Builtin.EqFloat -> relational FloatType
  Builtin.EqInt -> relational IntType
  Builtin.EqVector -> relational (VectorType AnyType)

  Builtin.Empty -> do
    popDataExpecting_ $ VectorType AnyType
    pushData BoolType

  Builtin.First -> do
    PairType a _ <- popDataExpecting $ PairType AnyType AnyType
    pushData a

  Builtin.Function -> do
    a <- popData
    pushData $ Composition [] :> Composition [a]

  Builtin.GeChar -> relational CharType
  Builtin.GeFloat -> relational FloatType
  Builtin.GeInt -> relational IntType
  Builtin.GeVector -> relational (VectorType AnyType)

  Builtin.Get -> do
    popDataExpecting_ IntType
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.GetLine -> do
    popDataExpecting_ HandleType
    pushData $ VectorType CharType

  Builtin.GtChar -> relational CharType
  Builtin.GtFloat -> relational FloatType
  Builtin.GtInt -> relational IntType
  Builtin.GtVector -> relational (VectorType AnyType)

  Builtin.IncFloat -> unary FloatType
  Builtin.IncInt -> unary IntType

  Builtin.LeChar -> relational CharType
  Builtin.LeFloat -> relational FloatType
  Builtin.LeInt -> relational IntType
  Builtin.LeVector -> relational (VectorType AnyType)

  Builtin.Length -> do
    popDataExpecting_ $ VectorType AnyType
    pushData IntType

  Builtin.LtChar -> relational CharType
  Builtin.LtFloat -> relational FloatType
  Builtin.LtInt -> relational IntType
  Builtin.LtVector -> relational (VectorType AnyType)

  Builtin.ModFloat -> binary FloatType
  Builtin.ModInt -> binary IntType

  Builtin.MulFloat -> binary FloatType
  Builtin.MulInt -> binary IntType

  Builtin.NeChar -> relational CharType
  Builtin.NeFloat -> relational FloatType
  Builtin.NeInt -> relational IntType
  Builtin.NeVector -> relational (VectorType AnyType)

  Builtin.NegFloat -> unary FloatType
  Builtin.NegInt -> unary IntType

  Builtin.NotBool -> unary BoolType

  Builtin.NotInt -> unary IntType

  Builtin.OpenIn -> do
    popDataExpecting_ $ VectorType CharType
    pushData HandleType

  Builtin.OpenOut -> do
    popDataExpecting_ $ VectorType CharType
    pushData HandleType

  Builtin.OrBool -> relational BoolType

  Builtin.OrInt -> binary IntType

  Builtin.Print -> do
    popDataExpecting_ HandleType
    popDataExpecting_ $ VectorType CharType

  Builtin.Rest -> do
    PairType _ b <- popDataExpecting $ PairType AnyType AnyType
    pushData b

  Builtin.Set -> do
    popDataExpecting_ IntType
    b <- popData
    VectorType a <- popDataExpecting $ VectorType AnyType
    if a == b
      then pushData $ VectorType a
      else mismatchedElements a b

  Builtin.ShowFloat -> do
    popDataExpecting_ FloatType
    pushData $ VectorType CharType

  Builtin.ShowInt -> do
    popDataExpecting_ IntType
    pushData $ VectorType CharType

  Builtin.Stderr -> pushData HandleType
  Builtin.Stdin -> pushData HandleType
  Builtin.Stdout -> pushData HandleType

  Builtin.SubFloat -> binary FloatType
  Builtin.SubInt -> binary IntType

  Builtin.Swap -> do
    b <- popData
    a <- popData
    pushData b
    pushData a

  Builtin.Top -> do
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Up -> do
    a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Vector -> do
    a <- popData
    pushData $ VectorType a

  Builtin.XorBool -> relational BoolType

  Builtin.XorInt -> binary IntType

  where

  unary a = do
    popDataExpecting_ a
    pushData a

  binary a = do
    popDataExpecting_ a
    popDataExpecting_ a
    pushData a

  relational a = do
    popDataExpecting_ a
    popDataExpecting_ a
    pushData BoolType

mismatchedElements
  :: Type Scalar
  -> Type Scalar
  -> Typecheck ()
mismatchedElements a b = typeError $ concat
  [ "mismatched element types "
  , show a
  , " and "
  , show b
  ]
