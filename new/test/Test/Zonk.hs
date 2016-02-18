{-# LANGUAGE OverloadedStrings #-}

module Test.Zonk
  ( spec
  ) where

import Kitten.Kind (Kind(..))
import Kitten.Type (Type(..), TypeId(..), Var(..))
import Test.Hspec (Spec, it, shouldBe)
import qualified Data.Map as Map
import qualified Kitten.Origin as Origin
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Zonk as Zonk

spec :: Spec
spec = do
  it "does nothing to free type variables" $ do
    Zonk.type_ TypeEnv.empty va `shouldBe` va
  it "substitutes one level" $ do
    Zonk.type_ TypeEnv.empty { TypeEnv.tvs = Map.singleton ia vb } va `shouldBe` vb
  it "substitutes multiple levels" $ do
    Zonk.type_ TypeEnv.empty { TypeEnv.tvs = Map.fromList [(ia, vb), (ib, int)] } va `shouldBe` int
  where
  o = Origin.point "" 0 0
  ia = TypeId 0
  va = TypeVar o $ Var ia kv
  ib = TypeId 1
  vb = TypeVar o $ Var ib kv
  kv = Value
  int = TypeConstructor o "int"
