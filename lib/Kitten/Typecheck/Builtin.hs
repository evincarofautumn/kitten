{-# LANGUAGE GADTs #-}

module Kitten.Typecheck.Builtin
  ( typecheckBuiltin
  ) where

import Control.Monad

import Kitten.Builtin (Builtin)
import Kitten.Type
import Kitten.Typecheck.Monad

import qualified Kitten.Builtin as Builtin

typecheckBuiltin :: Builtin -> Typecheck
typecheckBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat
  Builtin.AddInt -> intsToInt

  Builtin.AndBool -> boolsToBool

  Builtin.AndInt -> intsToInt

  Builtin.Apply -> do
    Composition consumption :> Composition production
      <- popDataExpecting $ AnyType :> AnyType
    mapM_ popDataExpecting_ $ reverse consumption
    mapM_ pushData production

  Builtin.At -> do
    popDataExpecting_ IntType
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Bottom -> do
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Cat -> do
    VectorType b <- popDataExpecting $ VectorType AnyType
    VectorType a <- popDataExpecting $ VectorType AnyType
    if a == b
      then pushData $ VectorType a
      else typeError $ concat
        [ "mismatched element types "
        , show a
        , " and "
        , show b
        , " in 'cat'"
        ]

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

  Builtin.DecFloat -> floatToFloat
  Builtin.DecInt -> intToInt

  Builtin.DivFloat -> floatsToFloat
  Builtin.DivInt -> intsToInt

  Builtin.Down -> do
    a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.EqFloat -> floatsToBool
  Builtin.EqInt -> intsToBool

  Builtin.Empty -> do
    popDataExpecting_ $ VectorType AnyType
    pushData BoolType

  Builtin.Function -> do
    a <- popData
    pushData $ Composition [] :> Composition [a]

  Builtin.GeFloat -> floatsToBool
  Builtin.GeInt -> intsToBool

  Builtin.GtFloat -> floatsToBool
  Builtin.GtInt -> intsToBool

  Builtin.IncFloat -> floatToFloat
  Builtin.IncInt -> intToInt

  Builtin.LeFloat -> floatsToBool
  Builtin.LeInt -> intsToBool

  Builtin.Length -> do
    popDataExpecting_ $ VectorType AnyType
    pushData IntType

  Builtin.LtFloat -> floatsToBool
  Builtin.LtInt -> intsToBool

  Builtin.ModFloat -> floatsToFloat
  Builtin.ModInt -> intsToInt

  Builtin.MulFloat -> floatsToFloat
  Builtin.MulInt -> intsToInt

  Builtin.NeFloat -> floatsToBool
  Builtin.NeInt -> intsToBool

  Builtin.NegFloat -> floatToFloat
  Builtin.NegInt -> intToInt

  Builtin.NotBool -> boolToBool

  Builtin.NotInt -> intToInt

  Builtin.OrBool -> boolsToBool

  Builtin.OrInt -> intsToInt

  Builtin.Print -> popDataExpecting_ TextType

  Builtin.ShowFloat -> do
    popDataExpecting_ FloatType
    pushData TextType

  Builtin.ShowInt -> do
    popDataExpecting_ IntType
    pushData TextType

  Builtin.SubFloat -> floatsToFloat
  Builtin.SubInt -> intsToInt

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

  Builtin.XorBool -> boolsToBool

  Builtin.XorInt -> intsToInt

  where

  boolToBool = do
    popDataExpecting_ BoolType
    pushData BoolType

  boolsToBool = do
    popDataExpecting_ BoolType
    popDataExpecting_ BoolType
    pushData BoolType

  floatToFloat = do
    popDataExpecting_ FloatType
    pushData FloatType

  intToInt = do
    popDataExpecting_ IntType
    pushData IntType

  floatsToFloat = do
    popDataExpecting_ FloatType
    popDataExpecting_ FloatType
    pushData FloatType

  intsToInt = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData IntType

  floatsToBool = do
    popDataExpecting_ FloatType
    popDataExpecting_ FloatType
    pushData BoolType

  intsToBool = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData BoolType
