{-# LANGUAGE OverloadedStrings #-}

module Test.Interpret
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Bits
import Kitten.Dictionary (Dictionary)
import Kitten.Interpret (interpret)
import Kitten.Monad (runKitten)
import Kitten.Name
import Kitten.Term (Value(..))
import System.IO (IOMode(..), hClose)
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, describe, it, runIO)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.ByteString as ByteString
import qualified Data.Knob as Knob
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
  testInterpretWithHandles <- runIO $ do
    commonSource <- IO.readFileUtf8 "common.ktn"
    mDictionary <- runKitten $ do
      common <- fragmentFromSource io Nothing 1 "<common>" commonSource
      Enter.fragment common Dictionary.empty
    case mDictionary of
      Left{} -> error "unable to set up interpreter tests"
      Right dictionary -> return $ testInterpretFull dictionary

  let testInterpret = testInterpretWithHandles "" Nothing Nothing

  describe "with trivial programs" $ do
    it "interprets literals" $ do
     testInterpret "0" [Integer 0 Signed32]
     testInterpret "0.0" [Float 0.0 Float64]
     testInterpret "1 2"
       [ Integer 2 Signed32
       , Integer 1 Signed32
       ]
     testInterpret "\"meow\""
       [Array $ Vector.fromList
         [Character 'm', Character 'e', Character 'o', Character 'w']]
    it "interprets 'hello world'" $ do
      testInterpret "\"meow\" say" []

  describe "with operators" $ do
    it "interprets Int32 operators" $ do
      testInterpret "2 + 3" [Integer 5 Signed32]
      testInterpret "2 - 3" [Integer (-1) Signed32]
      testInterpret "2 * 3" [Integer 6 Signed32]
      testInterpret "2 / 3" [Integer 0 Signed32]
      testInterpret "2 % 3" [Integer 2 Signed32]
    it "interprets chains of Int32 operators" $ do
      testInterpret "2 + 3 + 4" [Integer 9 Signed32]
      testInterpret "2 + 3 * 4" [Integer 14 Signed32]
      testInterpret "2 * 3 + 4" [Integer 10 Signed32]
      testInterpret "2 * 3 * 4" [Integer 24 Signed32]
    it "wraps Int32" $ do
      testInterpret "2147483647 + 1" [Integer (-2147483648) Signed32]
      testInterpret "-2147483648 - 1" [Integer 2147483647 Signed32]
    it "interprets Float64 operators" $ do
      testInterpret "2.0 + 3.0" [Float 5 Float64]
      testInterpret "2.0 - 3.0" [Float (-1) Float64]
      testInterpret "2.0 * 3.0" [Float 6 Float64]
      testInterpret "2.0 / 4.0" [Float 0.5 Float64]
      testInterpret "2.0 % 3.0" [Float 2 Float64]
    it "interprets Bool operators" $ do
      let
        false = Algebraic (ConstructorIndex 0) []
        true = Algebraic (ConstructorIndex 1) []
      testInterpret "false & false" [false]
      testInterpret "false & true" [false]
      testInterpret "true & false" [false]
      testInterpret "true & true" [true]
      testInterpret "false | false" [false]
      testInterpret "false | true" [true]
      testInterpret "true | false" [true]
      testInterpret "true | true" [true]
      testInterpret "false ~ false" [false]
      testInterpret "false ~ true" [true]
      testInterpret "true ~ false" [true]
      testInterpret "true ~ true" [false]
      testInterpret "false --> false" [true]
      testInterpret "false --> true" [true]
      testInterpret "true --> false" [false]
      testInterpret "true --> true" [true]

  describe "with scope" $ do
    it "looks up locals and closure values correctly" $ do
      testInterpretWithHandles
        ""
        (Just "1110\n1110\n")
        Nothing
        "1000 -> x1;\n\
        \100 -> y1;\n\
        \10\n\
        \{\n\
        \  -> a1;\n\
        \  (a1 + x1)\n\
        \  {\n\
        \    -> b1;\n\
        \    b1 + y1\n\
        \  } call\n\
        \} call\n\
        \say\n\
        \1000 -> x2;\n\
        \100 -> y2;\n\
        \10\n\
        \{\n\
        \  -> a2;\n\
        \  (a2 + y2)\n\
        \  {\n\
        \    -> b2;\n\
        \    b2 + x2\n\
        \  } call\n\
        \} call\n\
        \say\n\
        \\&"
        []

  describe "with common math words" $ do
    it "computes absolute values" $ do
      testInterpret "0 abs" [Integer 0 Signed32]
      testInterpret "+0 abs" [Integer 0 Signed32]
      testInterpret "-0 abs" [Integer 0 Signed32]
      testInterpret "1 abs" [Integer 1 Signed32]
      testInterpret "+1 abs" [Integer 1 Signed32]
      testInterpret "1000 abs" [Integer 1000 Signed32]
      testInterpret "+1000 abs" [Integer 1000 Signed32]
      testInterpret "-1000 abs" [Integer 1000 Signed32]
      testInterpret "2147483647 abs" [Integer 2147483647 Signed32]
      testInterpret "-2147483648 abs" [Integer (-2147483648) Signed32]

testInterpretFull
  :: Dictionary
  -> ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Text
  -> [Value ()]
  -> IO ()
testInterpretFull commonDictionary standardInput
  mExpectedStdout mExpectedStderr input expectedStack = do
  result <- runKitten $ do
    fragment <- fragmentFromSource io Nothing 1 "<test>" input
    Enter.fragment fragment commonDictionary
  (stdinKnob, stdin) <- do
    knob <- Knob.newKnob standardInput
    (,) knob <$> Knob.newFileHandle knob "stdin" ReadMode
  (stdoutKnob, stdout) <- do
    knob <- Knob.newKnob $ ByteString.pack []
    (,) knob <$> Knob.newFileHandle knob "stdout" WriteMode
  (stderrKnob, stderr) <- do
    knob <- Knob.newKnob $ ByteString.pack []
    (,) knob <$> Knob.newFileHandle knob "stderr" WriteMode
  case result of
    Right dictionary -> do
      actualStack <- map Term.stripValue
        <$> interpret dictionary Nothing [] stdin stdout stderr []
      hClose stdin
      hClose stdout
      hClose stderr
      assertEqual
        (Pretty.render $ Pretty.hsep
          ["stack", pPrint expectedStack, "=", pPrint actualStack])
        expectedStack actualStack
      case mExpectedStdout of
        Just expectedStdout -> do
          actualStdout <- Knob.getContents stdoutKnob
          assertEqual
            (Pretty.render $ Pretty.hsep
              [ "stdout"
              , Pretty.text $ show expectedStdout
              , "="
              , Pretty.text $ show actualStdout
              ])
            expectedStdout actualStdout
        Nothing -> return ()
      case mExpectedStderr of
        Just expectedStderr -> do
          actualStderr <- Knob.getContents stderrKnob
          assertEqual
            (Pretty.render $ Pretty.hsep
              [ "stderr"
              , Pretty.text $ show expectedStderr
              , "="
              , Pretty.text $ show actualStderr
              ])
            expectedStderr actualStderr
        Nothing -> return ()
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports

io :: [GeneralName]
io = [QualifiedName $ Qualified Vocabulary.global "IO"]
