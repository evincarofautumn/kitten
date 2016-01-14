{-# LANGUAGE OverloadedStrings #-}

module Test.Tokenize
  ( spec
  ) where

import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Kitten.Monad (runKitten)
import Kitten.Report (Report)
import Kitten.Token (Token(..))
import Kitten.Tokenize (tokenize)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Kitten.Located as Located
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report

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
    it "fails on unterminated block comment" $ do
      let origin = Origin.point "" 1 3
      testTokenize "/*" `shouldBe` Left
        [ Report.ParseError origin
          ["unexpected end of input"]
          "expected \"/*\" or \"*/\""
        ]
  describe "with single tokens" $ do
    it "produces single token for arrow" $ do
      testTokenize "->" `shouldBe` Right [Arrow]
  describe "with text literals" $ do
    it "produces empty text from empty text literal" $ do
      testTokenize "\"\"" `shouldBe` Right [Text ""]
    it "produces empty text from empty escape" $ do
      testTokenize "\"\\&\"" `shouldBe` Right [Text ""]
    it "collapses whitespace in text gaps" $ do
      testTokenize "\"\\    \"" `shouldBe` Right [Text " "]
      testTokenize "\"\\\n    \"" `shouldBe` Right [Text "\n"]
      testTokenize "\"\\\n\t x\"" `shouldBe` Right [Text "\nx"]
    it "parses correct text escapes" $ do
      testTokenize "\"\\a\\b\\f\\n\\r\\t\\v\"" `shouldBe` Right [Text "\a\b\f\n\r\t\v"]

testTokenize :: Text -> Either [Report] [Token]
testTokenize = fmap (map Located.item) . runIdentity . runKitten . tokenize ""
