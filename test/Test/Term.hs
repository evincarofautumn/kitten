module Test.Term where

import Control.Monad
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Builtin (Builtin)
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Term
import Kitten.Token (tokenize)

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" (Fragment [] [] [])

  describe "terms" $ do
    testTerm "1 2 3"
      (Fragment [] [] [int 1, int 2, int 3])
    testTerm "dup swap drop vec cat fun compose apply"
      $ Fragment [] []
        [ builtin Builtin.Dup
        , builtin Builtin.Swap
        , builtin Builtin.Drop
        , builtin Builtin.Vec
        , builtin Builtin.Cat
        , builtin Builtin.Fun
        , builtin Builtin.Compose
        , builtin Builtin.Apply
        ]

  describe "lambda" $ do

    testTerm
      "\\x x x *"
      $ Fragment [] []
        [ lambda "x"
          [ word "x"
          , word "x"
          , builtin Builtin.Mul
          ]
        ]

    testTerm
      "\\x \\y x y *"
      $ Fragment [] []
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , builtin Builtin.Mul
            ]
          ]
        ]

    testTerm
      "{ \\x \\y x y * }"
      $ Fragment [] []
        [ fun
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , builtin Builtin.Mul
              ]
            ]
          ]
        ]

    testTerm
      ": \\x \\y x y *"
      $ Fragment [] []
        [ fun
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , builtin Builtin.Mul
              ]
            ]
          ]
        ]

  describe "layout" $ do

    testTerm
      ": sameLine\n\
      \  nextLine\n\
      \  anotherLine\n"
      $ Fragment [] []
        [ fun
          [ word "sameLine"
          , word "nextLine"
          , word "anotherLine"
          ]
        ]

    testTerm
      "{ : sameLine\n\
      \    nextLine\n\
      \    anotherLine }\n"
      $ Fragment [] []
        [ fun
          [ fun
            [ word "sameLine"
            , word "nextLine"
            , word "anotherLine"
            ]
          ]
        ]

    testTerm "{ one : two three }"
      $ Fragment [] []
      [fun [word "one", fun [word "two", word "three"]]]

testTerm :: String -> Fragment Term -> Spec
testTerm source expected = it (show source)
  $ case parsed source of
    Left message -> assertFailure $ show message
    Right actual
      | expected == actual -> return ()
      | otherwise -> expectedButGot
        (show expected) (show actual)
  where
  parsed
    = liftParseError . parse "test"
    <=< liftParseError . tokenize "test"

builtin :: Builtin -> Term
builtin b = Builtin b TestLocation

compose :: [Term] -> Term
compose terms = Compose terms TestLocation

fun :: [Term] -> Term
fun terms = push $ Fun terms TestLocation

int :: Int -> Term
int value = push $ Int value TestLocation

push :: Value -> Term
push value = Push value TestLocation

lambda :: String -> [Term] -> Term
lambda name terms = Lambda name (compose terms) TestLocation

word :: String -> Term
word value = push $ Word value TestLocation
