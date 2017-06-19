{-# LANGUAGE OverloadedStrings #-}

module Test.Tokenize
  ( spec
  ) where

import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Kitten.Base (Base(..))
import Kitten.Bits
import Kitten.Monad (runKitten)
import Kitten.Name (Unqualified(..))
import Kitten.Report (Report)
import Kitten.Token (Token(..))
import Kitten.Tokenize (tokenize)
import Test.HUnit (assertFailure)
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
    it "rejects invalid whitespace" $ do
      -- TODO: Improve error message and test it.
      case testTokenize "\t" of
        Left _ -> return ()
        _ -> assertFailure "tabs should not be accepted"
      case testTokenize "\v" of
        Left _ -> return ()
        _ -> assertFailure "vertical tabs should not be accepted"
      case testTokenize "\f" of
        Left _ -> return ()
        _ -> assertFailure "form feeds should not be accepted"
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
    it "produces no tokens on nested multi-line block comment" $ do
      testTokenize
        "/*\n\
        \define useless<T> (T -> T):\n\
        \  /* FIXME: this is /* probably */ useless. */\n\
        \*/\n\
        \\&"
        `shouldBe` Right []
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
      testTokenize "\x2192" `shouldBe` Right [Arrow]
    it "produces single token for double colon" $ do
      testTokenize "::" `shouldBe` Right [VocabLookup]
      testTokenize "\x2237" `shouldBe` Right [VocabLookup]
    it "produces single token for ellipsis" $ do
      testTokenize "..." `shouldBe` Right [Ellipsis]
      testTokenize "\x2026" `shouldBe` Right [Ellipsis]
    it "produces single token for operator beginning with arrow" $ do
      testTokenize "->?" `shouldBe` Right
        [Operator (Unqualified "->?")]
      testTokenize "\x2192?" `shouldBe` Right
        [Operator (Unqualified "\x2192?")]

    -- This is to support stack quantifiers, e.g., "<R...>" should be parsed as:
    --
    --     <
    --     R
    --     ...
    --     >
    --
    -- And not:
    --
    --     <
    --     R
    --     ...>
    --
    it "produces multiple tokens for ellipsis followed by symbol" $ do
      testTokenize "...?" `shouldBe` Right
        [Ellipsis, Operator (Unqualified "?")]
      testTokenize "\x2026?" `shouldBe` Right
        [Ellipsis, Operator (Unqualified "?")]
    it "produces single tokens for adjacent single-char tokens" $ do
      testTokenize ",," `shouldBe` Right [Comma, Comma]

  describe "with text literals" $ do
    it "produces empty text from empty text literal" $ do
      testTokenize "\"\"" `shouldBe` Right [Text ""]
      testTokenize "\x201C\x201D" `shouldBe` Right [Text ""]
    it "produces empty text from empty escape" $ do
      testTokenize "\"\\&\"" `shouldBe` Right [Text ""]
      testTokenize "\x201C\\&\x201D" `shouldBe` Right [Text ""]
    it "collapses whitespace in text gaps" $ do
      testTokenize "\"\\    \"" `shouldBe` Right [Text " "]
      testTokenize "\"\\\n    \"" `shouldBe` Right [Text "\n"]
      testTokenize "\"\\\n\t x\"" `shouldBe` Right [Text "\nx"]
    it "parses correct text escapes" $ do
      testTokenize "\"\\a\\b\\f\\n\\r\\t\\v\"" `shouldBe` Right
        [Text "\a\b\f\n\r\t\v"]
    it "parses nested text" $ do
      testTokenize "\x201CHe said, \x201CWhat?\x201D\x201D" `shouldBe` Right
        [Text "He said, \x201CWhat?\x201D"]
    it "fails on unterminated text" $ do
      let origin = Origin.point "" 1 2
      testTokenize "\"" `shouldBe` Left
        [ Report.ParseError origin
          ["unexpected end of input"]
          "expected character, escape, or closing double quote"
        ]
    it "fails on unterminated nested text" $ do
      let origin = Origin.point "" 1 2
      testTokenize "\x201C" `shouldBe` Left
        [ Report.ParseError origin
          ["unexpected end of input"]
          "expected character, nested opening quote, \
          \escape, or closing right double quote"
        ]
    it "fails on multi-line text" $ do
      let origin = Origin.point "" 2 1
      testTokenize "\"\n\"" `shouldBe` Left
        [ Report.ParseError origin
          ["unexpected newline in text literal; \
            \use an escape, gap, or paragraph instead"]
          "expected character or escape"
        ]

  describe "with paragraph literals" $ do
    it "parses well-formed paragraph literals" $ do
      testTokenize
        "\"\"\"\n\
        \test\n\
        \\&\"\"\""
        `shouldBe` Right [Text "test"]
      testTokenize
        "\"\"\"\n\
        \  test\n\
        \  \"\"\""
        `shouldBe` Right [Text "test"]
      testTokenize
        "\"\"\"\n\
        \test\n\
        \test\n\
        \\&\"\"\""
        `shouldBe` Right [Text "test\ntest"]
      testTokenize
        "\"\"\"\n\
        \  test\n\
        \  test\n\
        \  \"\"\""
        `shouldBe` Right [Text "test\ntest"]
      testTokenize
        "\"\"\"\n\
        \  test\n\
        \  test\n\
        \\&\"\"\""
        `shouldBe` Right [Text "  test\n  test"]
      testTokenize
        "\"\"\"\n\
        \  This is a \"test\"  \n\
        \\&\"\"\""
        `shouldBe` Right [Text "  This is a \"test\"  "]
    it "rejects ill-formed paragraph literals" $ do
      let origin = Origin.point "" 4 6
      testTokenize
        "\"\"\"\n\
        \  foo\n\
        \ bar\n\
        \  \"\"\""
        `shouldBe` Left
        [ Report.ParseError origin
          ["unexpected \" bar\""]
          "expected all lines to be empty or begin with 2 spaces"
        ]
    -- TODO: Add more negative tests for paragraph literals.

  -- FIXME: Base hints are ignored in token comparisons.
  describe "with integer literals" $ do
    it "parses decimal integer literals" $ do
      testTokenize "0" `shouldBe` Right [Integer 0 Decimal Signed32]
      testTokenize "1" `shouldBe` Right [Integer 1 Decimal Signed32]
      testTokenize "10" `shouldBe` Right [Integer 10 Decimal Signed32]
      testTokenize "123456789" `shouldBe` Right [Integer 123456789 Decimal Signed32]
      testTokenize "01" `shouldBe` Right [Integer 1 Decimal Signed32]
    it "parses integer literals with sign characters" $ do
      testTokenize "+1" `shouldBe` Right [Integer 1 Decimal Signed32]
      testTokenize "-1" `shouldBe` Right [Integer (-1) Decimal Signed32]
      testTokenize "\x2212\&1" `shouldBe` Right [Integer (-1) Decimal Signed32]
    it "parses non-decimal integer literals" $ do
      testTokenize "0xFF" `shouldBe` Right [Integer 0xFF Hexadecimal Signed32]
      testTokenize "0o777" `shouldBe` Right [Integer 0o777 Octal Signed32]
      testTokenize "0b1010" `shouldBe` Right [Integer 10 Binary Signed32]

  describe "with floating-point literals" $ do
    it "parses normal float literals" $ do
      testTokenize "1.0" `shouldBe` Right [Float 10 1 0 Float64]
      testTokenize "1." `shouldBe` Right [Float 1 0 0 Float64]
    it "parses float literals with sign characters" $ do
      testTokenize "+1.0" `shouldBe` Right [Float 10 1 0 Float64]
      testTokenize "-1.0" `shouldBe` Right [Float (-10) 1 0 Float64]
      testTokenize "\x2212\&1.0" `shouldBe` Right [Float (-10) 1 0 Float64]
    it "parses float literals in scientific notation" $ do
      testTokenize "1.0e1" `shouldBe` Right [Float 10 1 1 Float64]
      testTokenize "1.e1" `shouldBe` Right [Float 1 0 1 Float64]
      testTokenize "1e1" `shouldBe` Right [Float 1 0 1 Float64]
      testTokenize "1e+1" `shouldBe` Right [Float 1 0 1 Float64]
      testTokenize "1e-1" `shouldBe` Right [Float 1 0 (-1) Float64]
      testTokenize "1e\x2212\&1" `shouldBe` Right [Float 1 0 (-1) Float64]

testTokenize :: Text -> Either [Report] [Token]
testTokenize = fmap (map Located.item) . runIdentity . runKitten
  . tokenize 1 ""
