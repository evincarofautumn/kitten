{-# LANGUAGE OverloadedStrings #-}

module Test.Tokenize
  ( spec
  ) where

import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Kitten (Origin(..), Report, Token(..), runKitten, tokenize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "with whitespace and comments" $ do
    it "produces no tokens on empty input" $ do
      testTokenize "" `shouldBe` Right []
    it "produces no tokens on only space" $ do
      testTokenize " " `shouldBe` Right []
    it "produces no tokens on only tab" $ do
      testTokenize "\t" `shouldBe` Right []
    it "produces no tokens on only line comment" $ do
      testTokenize "// comment" `shouldBe` Right []
    it "produces no tokens on only line comment plus newline" $ do
      testTokenize "// comment\n" `shouldBe` Right []
    it "produces no tokens on only block comment" $ do
      testTokenize "/* comment */" `shouldBe` Right []
    it "produces no tokens on empty block comment" $ do
      testTokenize "/**/" `shouldBe` Right []
    it "produces no tokens on nested empty block comment" $ do
      testTokenize "/*/**/*/" `shouldBe` Right []
    it "produces no tokens on nested spaced empty block comment" $ do
      testTokenize "/* /**/ */" `shouldBe` Right []
  describe "with single tokens" $ do
    it "produces single token for arrow" $ do
      testTokenize "->" `shouldBe` Right [ArrowToken Anywhere Nothing]

testTokenize :: Text -> Either [[Report]] [Token]
testTokenize = runIdentity . runKitten . tokenize ""
