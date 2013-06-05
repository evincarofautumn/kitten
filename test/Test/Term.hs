module Test.Term where

import Control.Monad
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Anno (Anno(Anno), Type((:>)))
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Parse
import Kitten.Term
import Kitten.Tokenize
import Kitten.Util.Either

import qualified Kitten.Anno as Anno

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" (Fragment [] [])

  describe "terms" $ do
    testTerm "1 2 3"
      (Fragment [] [pushi 1, pushi 2, pushi 3])
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
      "(->){}"
      $ Fragment []
        [ push $ function ([] :> []) []
        ]

    testTerm
      "(Int){3}"
      $ Fragment []
        [ push $ function
          ([] :> [Anno.Int])
          [pushi 3]
        ]

    testTerm
      "(Int -> Int){ 1 + }"
      $ Fragment []
        [ push $ function
          ([Anno.Int] :> [Anno.Int])
          [pushi 1, word "+"]
        ]

    {-
    testTerm
      "([a] (a -> b) -> [b]){ map }"
      $ Fragment []
        [ push $ function
          ([Anno.Vector Anno.Any, [Anno.Any] :> [Anno.Any]]
            :> [Anno.Vector Anno.Any])
          [word "map"]
        ]
    -}

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
      [Block [Block [pushi 3]]]

  describe "definition" $ do

    testTerm
      "def pi 3"
      $ Fragment [def "pi" (int 3)]
      []

    testTerm
      "def inc (Int -> Int){\n\
      \  1 +\n\
      \}\n"
      $ Fragment
        [ def "inc" $ function
          ([Anno.Int] :> [Anno.Int])
          [pushi 1, word "+"]
        ]
        []

    testTerm
      "def inc (Int -> Int):\n\
      \  1 +\n\
      \\n\
      \def dec (Int -> Int):\n\
      \  1 -\n\
      \\n"
      $ Fragment
        [ def "inc" $ function
          ([Anno.Int] :> [Anno.Int])
          [pushi 1, word "+"]
        , def "dec" $ function
          ([Anno.Int] :> [Anno.Int])
          [pushi 1, word "-"]
        ]
        []

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
def name value = Def name value TestLocation

function :: Anno.Type -> [Term] -> Value
function anno terms
  = Function (Just $ Anno anno TestLocation) terms TestLocation

int :: Int -> Value
int value = Int value TestLocation

pushi :: Int -> Term
pushi value = push $ int value

lambda :: String -> [Term] -> Term
lambda name terms = Lambda name terms TestLocation

push :: Value -> Term
push value = Push value TestLocation

word :: String -> Term
word value = Call value TestLocation
