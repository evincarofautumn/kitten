{-# LANGUAGE OverloadedStrings #-}

module Test.Token where

import Data.Text (Text)
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.Hspec
import Text.Parsec.Pos

import Test.Util

import Kitten.Location
import Kitten.Token
import Kitten.Tokenize

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "tokenize comment" $ do
    it "empty single-line comment"
      $ testComment "//\n"
    it "single-line comment"
      $ testComment "// Comment.\n"
    it "empty single-line comment ending at EOF"
      $ testComment "//"
    it "single-line comment ending at EOF"
      $ testComment "// Comment."
    it "multi-line comment on one line"
      $ testComment "/* Comment. */\n"
    it "multi-line comment on multiple lines"
      $ testComment "/*\nComment.\n*/\n"
    it "multi-line comment on multiple lines"
      $ testComment "/*\nComment.\n*/\n"
    it "multi-line comment on one line ending at EOF"
      $ testComment "/* Comment. */"
    it "multi-line comment on multiple lines ending at EOF"
      $ testComment "/*\nComment.\n*/"
    it "multi-line comment on multiple lines ending at EOF"
      $ testComment "/*\nComment.\n*/"
    it "nested multi-line comment"
      $ testComment "/* Nested /* multi-line */ comment. */"

  describe "single-character token" $ do
    testTokens "{" [BlockBegin NormalBlockHint]
    testTokens "}" [BlockEnd]
    testTokens "(" [GroupBegin]
    testTokens ")" [GroupEnd]
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

  describe "tokenize char" $ do
    testTokens "'\\''" [Char '\'']
    testTokens "'a'" [Char 'a']
    testTokens "'\\\"'" [Char '"']
    testTokens "'\\\\'" [Char '\\']

  describe "tokenize text" $ do
    testTokens "\"\"" [Text ""]
    testTokens "\"abc\"" [Text "abc"]
    testTokens "\"\\a\\b\\f\\n\\r\\t\\v\\\"\\\\\""
      [Text "\a\b\f\n\r\t\v\"\\"]

  describe "tokenize bool" $ do
    testTokens "true" [Bool True]
    testTokens "false" [Bool False]

  describe "tokenize keyword" $ do
    testTokens "\\" [Do]
    testTokens "def" [Def]
    testTokens "else" [Else]
    testTokens "import" [Import]

  describe "tokenize builtin" $ do
    testTokens "__add_int" [Builtin Builtin.AddInt]
    testTokens "__add_vector" [Builtin Builtin.AddVector]
    testTokens "__apply" [Builtin Builtin.Apply]

  describe "tokenize word" $ do
    testTokens "not_a_keyword" [Word "not_a_keyword"]
    testTokens "alsoNot" [Word "alsoNot"]
    testTokens "thisThat123" [Word "thisThat123"]
    testTokens "+-" [Operator "+-"]
    testTokens "<=>" [Operator "<=>"]
    testTokens "!#$%&*+-./<=>?@^|~"
      [Operator "!#$%&*+-./<=>?@^|~"]

  describe "locations" $ do
    testLocations 1 "1 2 3" [loc 1 1, loc 1 3, loc 1 5]
    testLocations 1 "def f {1} def f {2}" $ locs
      [ (1, 1), (1, 5), (1, 7), (1, 8), (1, 9), (1, 11), (1, 15), (1, 17)
      , (1, 18), (1, 19)
      ]
    testLocations 2 "def f {1}" $ locs
      [(2, 1), (2, 5), (2, 7), (2, 8), (2, 9)]
    testLocations 1 "  def f {1}" $ locs
      [(1, 3), (1, 7), (1, 9), (1, 10), (1, 11)]

locs :: [(Line, Column)] -> [Location]
locs = map (uncurry loc)

loc :: Line -> Column -> Location
loc line column = Location
  { locationStart = newPos "test" line column
  , locationIndent = -1
  }

testComment :: Text -> Assertion
testComment source = case tokenize 1 "test" source of
  Left message -> assertFailure $ show message
  Right [] -> return ()
  Right actual -> expectedButGot "[]" (showLocated actual)

testInt :: Text -> Int -> Spec
testInt source expected = it ("int " ++ show source)
  $ case tokenize 1 "test" source of
    Left message -> assertFailure $ show message
    Right [Located (Int actual _) _]
      | actual == expected -> return ()
    Right actual -> expectedButGot
      (show expected) (showLocated actual)

testTokens :: Text -> [Token] -> Spec
testTokens = testLocated locatedToken 1

testLocations :: Line -> Text -> [Location] -> Spec
testLocations = testLocated locatedLocation

testLocated
  :: (Eq a, Show a) => (Located -> a) -> Line -> Text -> [a] -> Spec
testLocated extract firstLine source expected = it (show source)
  $ case tokenize firstLine "test" source of
    Left message -> assertFailure $ show message
    Right actual
      | map extract actual == expected -> return ()
      | otherwise -> expectedButGot
        (showLines expected) (showLines actual')
      where actual' = map extract actual

showLocated :: [Located] -> String
showLocated = unwords . map (show . locatedToken)

showWords :: (Show a) => [a] -> String
showWords = unwords . map show

showLines :: (Show a) => [a] -> String
showLines = unlines . map show
