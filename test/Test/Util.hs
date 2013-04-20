module Test.Util
  ( expectedButGot
  ) where

import Test.HUnit.Lang (Assertion, assertFailure)

expectedButGot :: String -> String -> Assertion
expectedButGot expected actual = assertFailure $ unlines
  [ "\nExpected:"
  , expected
  , "\nActual:"
  , actual
  ]
