{-# LANGUAGE OverloadedStrings #-}

module Test.Parse
  ( spec
  ) where

import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Monad (runKitten)
import Test.Common
import Test.HUnit (Assertion, assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Report as Report
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  describe "with definitions" $ do

    it "accepts unqualified word as definition name" $ do
      testParse Positive "define word (->) {}"
    it "accepts qualified word as definition name" $ do
      testParse Positive "define vocabulary::word (->) {}"
    it "accepts unqualified word as definition name within vocab" $ do
      testParse Positive "vocab vocabulary { define word (->) {} }"
    it "accepts qualified word as definition name within vocab" $ do
      testParse Positive "vocab outer { define inner::word (->) {} }"

    it "accepts unqualified operator as definition name" $ do
      testParse Positive "define + (->) {}"
    it "accepts qualified operator as definition name" $ do
      testParse Positive "define vocabulary::+ (->) {}"
    it "accepts unqualified operator as definition name within vocab" $ do
      testParse Positive "vocab vocabulary { define + (->) {} }"
    it "accepts qualified operator as definition name within vocab" $ do
      testParse Positive "vocab outer { define inner::+ (->) {} }"

    it "accepts unqualified word as type name" $ do
      testParse Positive "type Word {}"
    it "accepts qualified word as type name" $ do
      testParse Positive "type vocabulary::Word {}"
    it "accepts unqualified word as type name within vocab" $ do
      testParse Positive "vocab vocabulary { type Word {} }"
    it "accepts qualified word as type name within vocab" $ do
      testParse Positive "vocab outer { type inner::Word {} }"

    it "rejects unqualified operator as type name" $ do
      testParse Negative "type + {}"
    it "rejects qualified operator as type name" $ do
      testParse Negative "type vocabulary::+ {}"

testParse :: Sign -> Text -> Assertion
testParse sign input = do
  result <- runKitten
    $ fragmentFromSource ioPermission Nothing 1 "<test>" input
  case result of
    Left reports -> case sign of
      Positive -> assertFailure $ unlines
        $ map (Pretty.render . Report.human) reports
      -- TODO: Test error messages for negative tests.
      Negative -> pure ()
    Right fragment -> case sign of
      Positive -> pure ()
      Negative -> assertFailure $ Pretty.render $ pPrint fragment
