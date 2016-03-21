{-# LANGUAGE OverloadedStrings #-}

module Test.Interpret
  ( spec
  ) where

import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Bits
import Kitten.Dictionary (Dictionary)
import Kitten.Interpret (interpret)
import Kitten.Monad (runKitten)
import Kitten.Name
import Kitten.Term (Value(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, describe, it, runIO)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Vector as Vector
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.IO as IO
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  testInterpret' <- runIO $ do
    commonSource <- IO.readFileUtf8 "common.ktn"
    mDictionary <- runKitten $ do
      common <- fragmentFromSource io Nothing 1 "<common>" commonSource
      Enter.fragment common Dictionary.empty
    case mDictionary of
      Left{} -> error "unable to set up interpreter tests"
      Right dictionary -> return $ testInterpret dictionary
  describe "with trivial programs" $ do
    it "interprets literals" $ do
     testInterpret' "0" [Integer 0 Signed32]
     testInterpret' "0.0" [Float 0.0 Float64]
     testInterpret' "1 2"
       [ Integer 2 Signed32
       , Integer 1 Signed32
       ]
     testInterpret' "\"meow\""
       [Array $ Vector.fromList
         [Character 'm', Character 'e', Character 'o', Character 'w']]
    it "interprets 'hello world'" $ do
     testInterpret' "\"meow\" say" []
  describe "with operators" $ do
    it "interprets Int32 operators" $ do
      testInterpret' "2 + 3" [Integer 5 Signed32]
      testInterpret' "2 - 3" [Integer (-1) Signed32]
      testInterpret' "2 * 3" [Integer 6 Signed32]
      testInterpret' "2 / 3" [Integer 0 Signed32]
      testInterpret' "2 % 3" [Integer 2 Signed32]
    it "interprets chains of Int32 operators" $ do
      testInterpret' "2 + 3 + 4" [Integer 9 Signed32]
      testInterpret' "2 + 3 * 4" [Integer 14 Signed32]
      testInterpret' "2 * 3 + 4" [Integer 10 Signed32]
      testInterpret' "2 * 3 * 4" [Integer 24 Signed32]
    it "wraps Int32" $ do
      testInterpret' "2147483647 + 1" [Integer (-2147483648) Signed32]
      testInterpret' "-2147483648 - 1" [Integer 2147483647 Signed32]
    it "interprets Float64 operators" $ do
      testInterpret' "2.0 + 3.0" [Float 5 Float64]
      testInterpret' "2.0 - 3.0" [Float (-1) Float64]
      testInterpret' "2.0 * 3.0" [Float 6 Float64]
      testInterpret' "2.0 / 4.0" [Float 0.5 Float64]
      testInterpret' "2.0 % 3.0" [Float 2 Float64]
    it "interprets Bool operators" $ do
      let
        false = Algebraic (ConstructorIndex 0) []
        true = Algebraic (ConstructorIndex 1) []
      testInterpret' "false & false" [false]
      testInterpret' "false & true" [false]
      testInterpret' "true & false" [false]
      testInterpret' "true & true" [true]
      testInterpret' "false | false" [false]
      testInterpret' "false | true" [true]
      testInterpret' "true | false" [true]
      testInterpret' "true | true" [true]
      testInterpret' "false ~ false" [false]
      testInterpret' "false ~ true" [true]
      testInterpret' "true ~ false" [true]
      testInterpret' "true ~ true" [false]
      testInterpret' "false --> false" [true]
      testInterpret' "false --> true" [true]
      testInterpret' "true --> false" [false]
      testInterpret' "true --> true" [true]

testInterpret :: Dictionary -> Text -> [Value ()] -> IO ()
testInterpret commonDictionary input expected = do
  result <- runKitten $ do
    fragment <- fragmentFromSource io Nothing 1 "<test>" input
    Enter.fragment fragment commonDictionary
  case result of
    Right dictionary -> do
      actual <- map Term.stripValue <$> interpret dictionary Nothing [] []
      assertEqual
        (Pretty.render $ Pretty.hsep [pPrint expected, "=", pPrint actual])
        expected actual
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports

io :: [GeneralName]
io = [QualifiedName $ Qualified Vocabulary.global "IO"]
