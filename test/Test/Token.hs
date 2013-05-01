module Test.Token where

import Test.HUnit.Lang (Assertion, assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Token

import qualified Kitten.Builtin as Builtin

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

  describe "single-character token" $ do
    testTokens "{" [BlockBegin]
    testTokens "}" [BlockEnd]
    testTokens "`" [Escape]
    testTokens "(" [GroupBegin]
    testTokens ")" [GroupEnd]
    testTokens "\\" [Lambda]
    testTokens ":" [Layout]
    testTokens "[" [VectorBegin]
    testTokens "]" [VectorEnd]

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

  describe "tokenize text" $ do
    testTokens "\"\"" [Text ""]
    testTokens "\"abc\"" [Text "abc"]
    testTokens "\"\\a\\b\\f\\n\\r\\t\\v\\\"\\\\\""
      [Text "\a\b\f\n\r\t\v\"\\"]

  describe "tokenize bool" $ do
    testTokens "true" [Bool True]
    testTokens "false" [Bool False]

  describe "tokenize keyword" $ do
    testTokens "bool" [BoolType]
    testTokens "int" [IntType]
    testTokens "text" [TextType]
    testTokens "def" [Def]

  describe "tokenize builtin" $ do
    testTokens "__dup" [Builtin Builtin.Dup]
    testTokens "__swap" [Builtin Builtin.Swap]
    testTokens "__drop" [Builtin Builtin.Drop]
    testTokens "__vector" [Builtin Builtin.Vector]
    testTokens "__cat" [Builtin Builtin.Cat]
    testTokens "__function" [Builtin Builtin.Function]
    testTokens "__compose" [Builtin Builtin.Compose]
    testTokens "__apply" [Builtin Builtin.Apply]

  describe "tokenize word" $ do
    testTokens "not_a_keyword" [Word "not_a_keyword"]
    testTokens "alsoNot" [Word "alsoNot"]
    testTokens "thisThat123" [Word "thisThat123"]
    testTokens "+-" [Word "+-"]
    testTokens "<=>" [Word "<=>"]
    testTokens "!#$%&*+,-./;<=>?@^|~"
      [Word "!#$%&*+,-./;<=>?@^|~"]

testComment :: String -> Assertion
testComment source = case tokenize "test" source of
  Left message -> assertFailure $ show message
  Right [] -> return ()
  Right actual -> expectedButGot "[]" (showLocated actual)

testInt :: String -> Int -> Spec
testInt source expected = it ("int " ++ show source)
  $ case tokenize "test" source of
    Left message -> assertFailure $ show message
    Right [Located (Int actual) _]
      | actual == expected -> return ()
    Right actual -> expectedButGot
      (show expected) (showLocated actual)

testTokens :: String -> [Token] -> Spec
testTokens source expected = it (show source)
  $ case tokenize "test" source of
    Left message -> assertFailure $ show message
    Right actual
      | map locatedToken actual == expected -> return ()
      | otherwise -> expectedButGot
        (showTokens expected) (showLocated actual)

showLocated :: [Located] -> String
showLocated = unwords . map (show . locatedToken)

showTokens :: [Token] -> String
showTokens = unwords . map show
