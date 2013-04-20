module Test.Term where

import Control.Monad
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import Test.Util

import Kitten.Anno (Anno)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Term
import Kitten.Token (tokenize)

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "empty program"
    $ testTerm "" (fragment [] [] [])

  describe "terms" $ do
    testTerm "1 2 3"
      (fragment [] [] [int 1, int 2, int 3])
    testTerm "dup swap drop vec cat fun compose apply"
      $ fragment [] []
        [ Builtin Builtin.Dup
        , Builtin Builtin.Swap
        , Builtin Builtin.Drop
        , Builtin Builtin.Vec
        , Builtin Builtin.Cat
        , Builtin Builtin.Fun
        , Builtin Builtin.Compose
        , Builtin Builtin.Apply
        ]

  describe "lambda" $ do

    testTerm
      "\\x x x *"
      $ fragment [] []
        [ lambda "x"
          [ word "x"
          , word "x"
          , Builtin Builtin.Mul
          ]
        ]

    testTerm
      "\\x \\y x y *"
      $ fragment [] []
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , Builtin Builtin.Mul
            ]
          ]
        ]

    testTerm
      "{ \\x \\y x y * }"
      $ fragment [] []
        [ fun
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , Builtin Builtin.Mul
              ]
            ]
          ]
        ]

    testTerm
      ": \\x \\y x y *"
      $ fragment [] []
        [ fun
          [ lambda "x"
            [ lambda "y"
              [ word "x"
              , word "y"
              , Builtin Builtin.Mul
              ]
            ]
          ]
        ]

  describe "layout" $ do

    testTerm
      ": sameLine\n\
      \  nextLine\n\
      \  anotherLine\n"
      $ fragment [] []
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
      $ fragment [] []
        [ fun
          [ fun
            [ word "sameLine"
            , word "nextLine"
            , word "anotherLine"
            ]
          ]
        ]

    testTerm "{ one : two three }"
      $ fragment [] []
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

fragment :: [Anno] -> [Def Term] -> [Term] -> Fragment Term
fragment annos defs terms
  = Fragment annos defs (Compose terms)

fun :: [Term] -> Term
fun = Value . Fun

int :: Int -> Term
int = Value . Int

lambda :: String -> [Term] -> Term
lambda name terms = Lambda name $ Compose terms

word :: String -> Term
word = Value . Word
