module Main
  ( main
  ) where

import Test.Hspec (Spec, describe, hspec)
import qualified Test.Infer
import qualified Test.InstanceCheck
import qualified Test.Interpret
import qualified Test.Origin
import qualified Test.Parse
import qualified Test.Resolve
import qualified Test.Tokenize
import qualified Test.Zonk

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tokenization" Test.Tokenize.spec
  describe "source locations" Test.Origin.spec
  describe "parsing" Test.Parse.spec
  describe "name resolution" Test.Resolve.spec
  describe "instance checking" Test.InstanceCheck.spec
  describe "type inference" Test.Infer.spec
  describe "zonking" Test.Zonk.spec
  describe "interpretation" Test.Interpret.spec
