module Main
  ( main
  ) where

import Test.Hspec (Spec, describe, hspec)
import qualified Test.Tokenize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tokenization" Test.Tokenize.spec
