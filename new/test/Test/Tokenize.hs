{-# LANGUAGE OverloadedStrings #-}

module Test.Tokenize
  ( spec
  ) where

import Data.Functor.Identity (runIdentity)
import Kitten (runKitten, tokenize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "trivial" $ do
    it "produces no tokens on empty input" $ do
      runIdentity (runKitten (tokenize "" "")) `shouldBe` Right []
