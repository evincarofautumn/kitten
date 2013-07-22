module Test.Term where

import Control.Monad
import Data.Monoid
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Parse
import Kitten.Term
import Kitten.Tokenize
import Kitten.Util.Either

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" mempty

  describe "terms" $ do
    testTerm "1 2 3"
      mempty { fragmentTerms = [pushi 1, pushi 2, pushi 3] }
    testTerm "dup swap drop vector cat function compose"
      $ mempty { fragmentTerms =
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
      $ mempty { fragmentTerms =
        [push $ function []] }

    testTerm
      "{3}"
      $ mempty { fragmentTerms =
        [push $ function [pushi 3]] }

    testTerm
      "{ 1 + }"
      $ mempty { fragmentTerms =
        [push $ function [pushi 1, word "+"]] }

  describe "lambda" $ do

    testTerm
      "->x x x *"
      $ mempty { fragmentTerms =
        [ lambda "x"
          [ word "x"
          , word "x"
          , word "*"]] }

    testTerm
      "->x ->y x y *"
      $ mempty { fragmentTerms =
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , word "*"]]] }

    testTerm
      "{ ->x ->y x y * }"
      $ mempty { fragmentTerms =
        [ push $ function
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , word "*"]]]] }

    testTerm
      ": ->x ->y x y *"
      $ mempty { fragmentTerms =
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
      $ mempty { fragmentTerms =
        [ push $ function
          [ word "sameLine"
          , word "nextLine"
          , word "anotherLine"]] }

    testTerm
      "{ : sameLine\n\
      \    nextLine\n\
      \    anotherLine }\n"
      $ mempty { fragmentTerms =
        [ push $ function
          [ push $ function
            [ word "sameLine"
            , word "nextLine"
            , word "anotherLine"]]] }

    testTerm "{ one : two three }"
      $ mempty { fragmentTerms =
        [ push $ function
          [ word "one"
          , push $ function
            [ word "two"
            , word "three"]]] }

    testTerm ": {one} {two}"
      $ mempty { fragmentTerms =
        [ push $ function
          [ push $ function [word "one"]
          , push $ function [word "two"]]] }

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
      $ mempty { fragmentTerms =
        [push $ function [push $ function [pushi 3]]] }

  describe "definition" $ do

    testTerm
      "def pi 3"
      $ mempty { fragmentDefs =
        [def "pi" $ function [pushi 3]] }

    testTerm
      "def inc {\n\
      \  1 +\n\
      \}\n"
      $ mempty { fragmentDefs =
        [def "inc" $ function [pushi 1, word "+"]] }

    testTerm
      "def inc:\n\
      \  1 +\n\
      \\n\
      \def dec:\n\
      \  1 -\n\
      \\n"
      $ mempty { fragmentDefs =
        [ def "inc" $ function [pushi 1, word "+"]
        , def "dec" $ function [pushi 1, word "-"]] }

testTerm :: String -> Fragment Value Term -> Spec
testTerm source expected = it (show source)
  $ case parsed source of
    Left message -> assertFailure $ show message
    Right actual
      | expected == actual -> return ()
      | otherwise -> expectedButGot
        (show expected) (show actual)

testTermFailure :: String -> Spec
testTermFailure source = it ("should fail: " ++ show source)
  $ case parsed source of
    Left _ -> return ()
    Right actual -> assertFailure $ show actual

parsed :: String -> Either CompileError (Fragment Value Term)
parsed
  = mapLeft parseError . tokenize "test"
  >=> mapLeft parseError . parse "test"

def :: String -> Value -> Def Value
def name value = Def
  { defName = name
  , defAnno = Nothing
  , defTerm = value
  , defLocation = TestLocation
  }

function :: [Term] -> Value
function terms = Function terms TestLocation

int :: Int -> Value
int value = Int value TestLocation

pushi :: Int -> Term
pushi value = push $ int value

lambda :: String -> [Term] -> Term
lambda name terms
  = Lambda name (Compose terms TestLocation) TestLocation

push :: Value -> Term
push value = Push value TestLocation

word :: String -> Term
word value = Call value TestLocation
