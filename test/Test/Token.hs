module Test.Token where

import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Kitten.Token

spec :: Spec
spec = do
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

testInt :: String -> Int -> Spec
testInt source expected = it ("int " ++ show source)
  $ case tokenize "test" source of
    Left message -> assertFailure $ show message
    Right [Located _ _ (Int actual)]
      | actual == expected -> return ()
    Right actual -> assertFailure $ unwords
      [ "expected"
      , show expected
      , "but got"
      , show actual
      ]
