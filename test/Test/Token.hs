module Test.Token where

import Test.HUnit.Lang (Assertion, assertFailure)
import Test.Hspec

import Kitten.Token

spec :: Spec
spec = do
  describe "tokenize comment" $ do
    it "empty single-line comment"
      $ testComment "--\n"
    it "single-line comment"
      $ testComment "-- Comment.\n"
    it "empty single-line comment ending at EOF"
      $ testComment "--"
    it "single-line comment ending at EOF"
      $ testComment "-- Comment."
    it "multi-line comment on one line"
      $ testComment "{- Comment. -}\n"
    it "multi-line comment on multiple lines"
      $ testComment "{-\nComment.\n-}\n"
    it "multi-line comment on multiple lines"
      $ testComment "{-\nComment.\n-}\n"
    it "multi-line comment on one line ending at EOF"
      $ testComment "{- Comment. -}"
    it "multi-line comment on multiple lines ending at EOF"
      $ testComment "{-\nComment.\n-}"
    it "multi-line comment on multiple lines ending at EOF"
      $ testComment "{-\nComment.\n-}"
    it "nested multi-line comment"
      $ testComment "{- Nested {- multi-line -} comment. -}"

  describe "tokenize int" $ do
    testInt "0" 0
    testInt "5" 5
    testInt "10" 10
    testInt "123" 123
    testInt "+0" 0
    testInt "-0" 0
    testInt "+5" 5
    testInt "-5" (-5)
    testInt "+10" 10
    testInt "-10" (-10)
    testInt "+123" 123
    testInt "-123" (-123)

testComment :: String -> Assertion
testComment source = case tokenize "test" source of
  Left message -> assertFailure $ show message
  Right [] -> return ()
  Right actual -> expectedButGot "[]" actual

testInt :: String -> Int -> Spec
testInt source expected = it ("int " ++ show source)
  $ case tokenize "test" source of
    Left message -> assertFailure $ show message
    Right [Located _ _ (Int actual)]
      | actual == expected -> return ()
    Right actual -> expectedButGot expected actual

expectedButGot :: (Show a, Show b) => a -> b -> Assertion
expectedButGot expected actual = assertFailure $ unwords
  [ "expected"
  , show expected
  , "but got"
  , show actual
  ]
