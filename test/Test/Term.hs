module Test.Term where

import Control.Monad
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Anno (Anno(Anno), Type((:>)))
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Kind
import Kitten.Location
import Kitten.Parse
import Kitten.Term
import Kitten.Tokenize

import qualified Kitten.Anno as Anno

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" (Fragment [] [])

  describe "terms" $ do
    testTerm "1 2 3"
      (Fragment [] [int 1, int 2, int 3])
    testTerm "dup swap drop vector cat function compose apply"
      $ Fragment []
        [ word "dup"
        , word "swap"
        , word "drop"
        , word "vector"
        , word "cat"
        , word "function"
        , word "compose"
        , word "apply"
        ]

  describe "function" $ do

    testTerm
      "(){}"
      $ Fragment []
        [ push $ function
          (Anno.Composition [] :> Anno.Composition [])
          []
        ]

    testTerm
      "(int){3}"
      $ Fragment []
        [ push $ function
          (Anno.Composition [] :> Anno.Composition [Anno.Int])
          [int 3]
        ]

    testTerm
      "(int -> int){ 1 + }"
      $ Fragment []
        [ push $ function
          (Anno.Composition [Anno.Int] :> Anno.Composition [Anno.Int])
          [int 1, word "+"]
        ]

    testTerm
      "([a] (a -> b) -> [b]){ map }"
      $ Fragment []
        [ push $ function
          (Anno.Composition
            [Anno.Vector Anno.Any, Anno.Any :> Anno.Any]
            :> Anno.Composition [Anno.Vector Anno.Any])
          [word "map"]
        ]

  describe "lambda" $ do

    testTerm
      "->x x x *"
      $ Fragment []
        [ lambda "x"
          [ word "x"
          , word "x"
          , word "*"
          ]
        ]

    testTerm
      "->x ->y x y *"
      $ Fragment []
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , word "*"
            ]
          ]
        ]

    testTerm
      "{ ->x ->y x y * }"
      $ Fragment []
        [ Block
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , word "*"
              ]
            ]
          ]
        ]

    testTerm
      ": ->x ->y x y *"
      $ Fragment []
        [ Block
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , word "*"
              ]
            ]
          ]
        ]

  describe "layout" $ do

    testTerm
      ": sameLine\n\
      \  nextLine\n\
      \  anotherLine\n"
      $ Fragment []
        [ Block
          [ word "sameLine"
          , word "nextLine"
          , word "anotherLine"
          ]
        ]

    testTerm
      "{ : sameLine\n\
      \    nextLine\n\
      \    anotherLine }\n"
      $ Fragment []
        [ Block
          [ Block
            [ word "sameLine"
            , word "nextLine"
            , word "anotherLine"
            ]
          ]
        ]

    testTerm "{ one : two three }"
      $ Fragment []
      [Block [word "one", Block [word "two", word "three"]]]

    testTerm ": {one} {two}"
      $ Fragment []
      [Block [Block [word "one"], Block [word "two"]]]

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
      $ Fragment []
      [Block [Block [int 3]]]

  describe "definition" $ do

    testTerm
      "def pi 3"
      $ Fragment [def "pi" (int 3)]
      []

    testTerm
      "def inc (int -> int){\n\
      \  1 +\n\
      \}\n"
      $ Fragment
        [ def "inc" . push $ function
          (Anno.Composition [Anno.Int] :> Anno.Composition [Anno.Int])
          [int 1, word "+"]
        ]
        []

    testTerm
      "def inc (int -> int):\n\
      \  1 +\n\
      \\n\
      \def dec (int -> int):\n\
      \  1 -\n\
      \\n"
      $ Fragment
        [ def "inc" . push $ function
          (Anno.Composition [Anno.Int] :> Anno.Composition [Anno.Int])
          [int 1, word "+"]
        , def "dec" . push $ function
          (Anno.Composition [Anno.Int] :> Anno.Composition [Anno.Int])
          [int 1, word "-"]
        ]
        []

testTerm :: String -> Fragment Term -> Spec
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

parsed :: String -> Either CompileError (Fragment Term)
parsed
  = liftParseError . tokenize "test"
  >=> liftParseError . parse "test"

def :: String -> Term -> Def Term
def name term = Def name term TestLocation

function :: Anno.Type Scalar -> [Term] -> Value
function anno terms
  = Function (Anno anno TestLocation) terms TestLocation

int :: Int -> Term
int value = push $ Int value TestLocation

lambda :: String -> [Term] -> Term
lambda name terms = Lambda name terms TestLocation

push :: Value -> Term
push value = Push value TestLocation

word :: String -> Term
word value = push $ Word value TestLocation
