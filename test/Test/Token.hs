{-# LANGUAGE OverloadedStrings #-}

module Test.Token where

import Data.Text (Text)
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.Hspec
import Text.Parsec.Pos

import Test.Util

import Kitten.Location
import Kitten.Tokenize
import Kitten.Types
import Kitten.Util.Monad

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
    testTokens "{" [TkBlockBegin NormalBlockHint]
    testTokens "}" [TkBlockEnd]
    testTokens "(" [TkGroupBegin]
    testTokens ")" [TkGroupEnd]
    testTokens ":" [TkLayout]
    testTokens "[" [TkVectorBegin]
    testTokens "]" [TkVectorEnd]

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
    testTokens "'\\''" [TkChar '\'']
    testTokens "'a'" [TkChar 'a']
    testTokens "'\\\"'" [TkChar '"']
    testTokens "'\\\\'" [TkChar '\\']

  describe "tokenize text" $ do
    testTokens "\"\"" [TkText ""]
    testTokens "\"abc\"" [TkText "abc"]
    testTokens "\"\\a\\b\\f\\n\\r\\t\\v\\\"\\\\\""
      [TkText "\a\b\f\n\r\t\v\"\\"]

  describe "tokenize bool" $ do
    testTokens "true" [TkBool True]
    testTokens "false" [TkBool False]

  describe "tokenize keyword" $ do
    testTokens "define" [TkDefine]
    testTokens "import" [TkImport]

  describe "tokenize builtin" $ do
    testTokens "__add_int" [TkIntrinsic InAddInt]
    testTokens "__add_vector" [TkIntrinsic InAddVector]
    testTokens "__apply" [TkIntrinsic InApply]

  describe "tokenize word" $ do
    testTokens "not_a_keyword" [TkWord "not_a_keyword"]
    testTokens "alsoNot" [TkWord "alsoNot"]
    testTokens "thisThat123" [TkWord "thisThat123"]
    testTokens "+-" [TkOperator "+-"]
    testTokens "<=>" [TkOperator "<=>"]
    testTokens "!#$%&*+-./<=>?@^|~"
      [TkOperator "!#$%&*+-./<=>?@^|~"]

  describe "locations" $ do
    testLocations 1 "1 2 3" [loc 1 1, loc 1 3, loc 1 5]
    testLocations 1 "define f {1} define f {2}" $ locs
      [ (1, 1), (1, 8), (1, 10), (1, 11), (1, 12), (1, 14), (1, 21), (1, 23)
      , (1, 24), (1, 25)
      ]
    testLocations 2 "define f {1}" $ locs
      [(2, 1), (2, 8), (2, 10), (2, 11), (2, 12)]
    testLocations 1 "  define f {1}" $ locs
      [(1, 3), (1, 10), (1, 12), (1, 13), (1, 14)]

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
  Right [] -> noop
  Right actual -> expectedButGot "[]" (showLocated actual)

testInt :: Text -> Int -> Spec
testInt source expected = it ("int " ++ show source)
  $ case tokenize 1 "test" source of
    Left message -> assertFailure $ show message
    Right [Located (TkInt actual _) _]
      | actual == expected -> noop
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
      | map extract actual == expected -> noop
      | otherwise -> expectedButGot
        (showLines expected) (showLines actual')
      where actual' = map extract actual

showLocated :: [Located] -> String
showLocated = unwords . map (show . locatedToken)

showWords :: (Show a) => [a] -> String
showWords = unwords . map show

showLines :: (Show a) => [a] -> String
showLines = unlines . map show
