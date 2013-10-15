{-# LANGUAGE OverloadedStrings #-}

module Test.Term where

import Control.Monad
import Data.Monoid
import Test.HUnit.Lang (assertFailure)
import Test.Hspec
import Data.Text (Text)

import qualified Data.Vector as V

import Kitten.Anno (Anno(Anno))
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Parse
import Kitten.Term
import Kitten.Tokenize
import Kitten.Util.Either
import Test.Util

import qualified Kitten.Anno as Anno
import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" mempty

  describe "terms" $ do
    testTerm "1 2 3"
      mempty { fragmentTerms = V.fromList [pushi 1, pushi 2, pushi 3] }
    testTerm "dup swap drop vector cat function compose"
      $ mempty { fragmentTerms = V.fromList
        [ word "dup"
        , word "swap"
        , word "drop"
        , word "vector"
        , word "cat"
        , word "function"
        , word "compose"
        ] }

  describe "function" $ do

    testTerm
      "{}"
      $ mempty { fragmentTerms = V.fromList
        [push $ function []] }

    testTerm
      "{3}"
      $ mempty { fragmentTerms = V.fromList
        [push $ function [pushi 3]] }

    testTerm
      "{ 1 + }"
      $ mempty { fragmentTerms = V.fromList
        [push $ function [pushi 1, word "+"]] }

  describe "lambda" $ do

    testTerm
      "->x x x *"
      $ mempty { fragmentTerms = V.fromList
        [ lambda "x"
          [ word "x"
          , word "x"
          , word "*"]] }

    testTerm
      "->x ->y x y *"
      $ mempty { fragmentTerms = V.fromList
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , word "*"]]] }

    testTerm
      "{ ->x ->y x y * }"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , word "*"]]]] }

    testTerm
      ": ->x ->y x y *"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , word "*"]]]] }

  describe "layout" $ do

    testTerm
      ": sameLine\n\
      \  nextLine\n\
      \  anotherLine\n"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ word "sameLine"
          , word "nextLine"
          , word "anotherLine"]] }

    testTerm
      "{ : sameLine\n\
      \    nextLine\n\
      \    anotherLine }\n"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ push $ function
            [ word "sameLine"
            , word "nextLine"
            , word "anotherLine"]]] }

    testTerm "{ one : two three }"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ word "one"
          , push $ function
            [ word "two"
            , word "three"]]] }

    testTerm ": {one} {two}"
      $ mempty { fragmentTerms = V.fromList
        [ push $ function
          [ push $ function [word "one"]
          , push $ function [word "two"]]] }

    testTerm
      "\\option (0 some):\n\
      \  drop // This comment is necessary.\n\
      \else:\n\
      \  noop\n"
      $ mempty { fragmentTerms = V.fromList
        [ compose [compose [pushi 0, call "some"]
        , push $ function [call "drop"]
        , push $ function [call "noop"]
        , Builtin Builtin.OptionElse TestLocation]] }

    testTermFailure ":"

    testTermFailure
      "  :\n\
      \3\n"

    testTermFailure
      ":\n\
      \3\n"

    testTerm
      ": :\n\
      \  3\n"
      $ mempty { fragmentTerms = V.fromList
        [push $ function [push $ function [pushi 3]]] }

  describe "definition" $ do

    testTerm
      "def pi: 3"
      $ mempty { fragmentDefs = V.fromList
        [def "pi" $ compose [pushi 3]] }

    testTerm
      "def inc {\n\
      \  1 +\n\
      \}\n"
      $ mempty { fragmentDefs = V.fromList
        [def "inc" $ compose [pushi 1, word "+"]] }

    testTerm
      "def inc:\n\
      \  1 +\n\
      \\n\
      \def dec:\n\
      \  1 -\n\
      \\n"
      $ mempty { fragmentDefs = V.fromList
        [ def "inc" $ compose [pushi 1, word "+"]
        , def "dec" $ compose [pushi 1, word "-"]] }

  describe "type" $ do
    testTerm
      "def curriedAdd (Int -> Int -> Int) {\n\
      \  ->x\n\
      \  { ->y x y + }\n\
      \}"
      $ mempty { fragmentDefs = V.fromList
        [ defWithAnno "curriedAdd"
          (Anno.Function
            (V.fromList [Anno.Int])
            (V.fromList
              [Anno.Function
                (V.fromList [Anno.Int])
                (V.fromList [Anno.Int])
                Anno.NoEffect])
            Anno.NoEffect)
          $ compose
            [ lambda "x"
              [ push $ function
                [ lambda "y"
                  [word "x", word "y", word "+"]]]]] }

testTerm :: Text -> Fragment Term -> Spec
testTerm source expected = it (show source)
  $ case parsed source of
    Left message -> assertFailure $ show message
    Right actual
      | expected == actual -> return ()
      | otherwise -> expectedButGot
        (show expected) (show actual)

testTermFailure :: Text -> Spec
testTermFailure source = it ("should fail: " ++ show source)
  $ case parsed source of
    Left _ -> return ()
    Right actual -> assertFailure $ show actual

parsed :: Text -> Either ErrorGroup (Fragment Term)
parsed
  = mapLeft parseError . tokenize 1 "test"
  >=> mapLeft parseError . parse "test"

def :: Text -> a -> Def a
def name term = Def
  { defName = name
  , defAnno = Nothing
  , defTerm = term
  , defLocation = TestLocation
  }

defWithAnno :: Text -> Anno.Type -> a -> Def a
defWithAnno name anno term = (def name term)
  { defAnno = Just (Anno anno TestLocation) }

call :: Text -> Term
call name = Call name TestLocation

compose :: [Term] -> Term
compose terms = Compose (V.fromList terms) TestLocation

function :: [Term] -> Value
function terms = Function (V.fromList terms) TestLocation

int :: Int -> Value
int value = Int value TestLocation

pushi :: Int -> Term
pushi value = push $ int value

lambda :: Text -> [Term] -> Term
lambda name terms
  = Lambda name (compose terms) TestLocation

push :: Value -> Term
push value = Push value TestLocation

word :: Text -> Term
word value = Call value TestLocation
