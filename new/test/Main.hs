module Main
  ( main
  ) where

import Test.Hspec (Spec, describe, hspec)
import qualified Test.Infer
import qualified Test.Origin
import qualified Test.Tokenize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tokenization" Test.Tokenize.spec
  describe "source locations" Test.Origin.spec
  describe "type inference" Test.Infer.spec
