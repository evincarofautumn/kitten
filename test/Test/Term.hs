{-# LANGUAGE OverloadedStrings #-}

module Test.Term where

import Control.Arrow
import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Test.HUnit.Lang (assertFailure)
import Test.Hspec

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Annotation
import Kitten.Definition
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Parse
import Kitten.Program
import Kitten.Term
import Kitten.Tokenize
import Kitten.Type
import Kitten.Util.Either
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..))
import Test.Util

spec :: Spec
spec = do
  describe "empty program"
    . testTerm "" . termFragment $ compose []

  describe "terms" $ do
    testTerm "1 2 3"
      . termFragment $ compose [pushi 1, pushi 2, pushi 3]
    testTerm "dup swap drop vector cat function compose"
      . termFragment $ compose
        [ word "dup"
        , word "swap"
        , word "drop"
        , word "vector"
        , word "cat"
        , word "function"
        , word "compose"
        ]

  describe "function" $ do

    testTerm
      "{}"
      . termFragment . push $ quotation []

    testTerm
      "{3}"
      . termFragment . push $ quotation [pushi 3]

    testTerm
      "{ 1 (+) }"
      . termFragment . push $ quotation [pushi 1, word "+"]

  describe "lambda" $ do

    testTerm
      "->x; x x (*)"
      . termFragment $ lambda "x"
        [ word "x"
        , word "x"
        , word "*" ]

    testTerm
      "->x; ->y; x y (*)"
      . termFragment $ lambda "x"
        [ lambda "y"
          [ word "x"
          , word "y"
          , word "*" ] ]

    testTerm
      "{ ->x; ->y; x y (*) }"
      . termFragment . push $ quotation
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , word "*" ] ] ]

    testTerm
      ": ->x; ->y; x y (*)"
      . termFragment . push $ quotation
        [ lambda "x"
          [ lambda "y"
            [ word "x"
            , word "y"
            , word "*" ] ] ]

  describe "layout" $ do

    testTerm
      ": sameLine\n\
      \  nextLine\n\
      \  anotherLine\n"
      . termFragment . push $ quotation
        [ word "sameLine"
        , word "nextLine"
        , word "anotherLine" ]

    testTerm
      "{ : sameLine\n\
      \    nextLine\n\
      \    anotherLine }\n"
      . termFragment . push $ quotation
        [ push $ quotation
          [ word "sameLine"
          , word "nextLine"
          , word "anotherLine" ] ]

    testTerm "{ one : two three }"
      . termFragment . push $ quotation
        [ word "one"
        , push $ quotation
          [ word "two"
          , word "three" ] ]

    testTerm ": {one} {two}"
      . termFragment . push $ quotation
        [ push $ quotation [word "one"]
        , push $ quotation [word "two"] ]

    let
      optionElseName = MixfixName
        [ "option"
        , MixfixHole
        , MixfixHole
        , "else"
        , MixfixHole
        ]

    testTerm
      "define option()()else() (->) {}\n\
      \option (0 some):\n\
      \  drop // This comment is necessary.\n\
      \else:\n\
      \  noop\n"
      (termFragment $ compose
        [ compose [compose [pushi 0, call "some"]
        , push $ quotation [call "drop"]
        , push $ quotation [call "noop"]
        , word optionElseName ] ])
        { fragmentDefs = defList
          [ defWithAnno optionElseName
            (AnFunction V.empty V.empty TestLocation)
            $ compose [] ] }

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
      . termFragment . push $ quotation [push $ quotation [pushi 3]]

    testTerm
      ": : 1\n\
      \    2\n\
      \  3\n"
      . termFragment . push $ quotation
        [push $ quotation [pushi 1, pushi 2], pushi 3]


  describe "definition" $ do

    testTerm
      "define pi (-> Float): 3"
      $ emptyFragment { fragmentDefs = defList
        [def (Qualified (Qualifier V.empty) "pi") $ compose [pushi 3]] }

    testTerm
      "define inc (int -> int) {\n\
      \  1 (+)\n\
      \}\n"
      $ emptyFragment { fragmentDefs = defList
        [def (Qualified (Qualifier V.empty) "inc") $ compose [pushi 1, word "+"]] }

    testTerm
      "define inc (int -> int):\n\
      \  1 (+)\n\
      \\n\
      \define dec (int -> int):\n\
      \  1 (-)\n\
      \\n"
      $ emptyFragment { fragmentDefs = defList
        [ def (Qualified (Qualifier V.empty) "inc") $ compose [pushi 1, word "+"]
        , def (Qualified (Qualifier V.empty) "dec") $ compose [pushi 1, word "-"] ] }

  describe "type" $ testTerm
    "define curriedAdd (int -> int -> int) {\n\
    \  ->x;\n\
    \  { ->y; x y (+) }\n\
    \}"
    $ emptyFragment { fragmentDefs = defList
      [ defWithAnno (Qualified (Qualifier V.empty) "curriedAdd")
        (AnFunction
          (V.fromList [AnVar "int" TestLocation])
          (V.fromList
            [AnFunction
              (V.fromList [AnVar "int" TestLocation])
              (V.fromList [AnVar "int" TestLocation]) TestLocation]) TestLocation)
        $ compose
          [ lambda "x"
            [ push $ quotation
              [ lambda "y"
                [word "x", word "y", word "+"] ] ] ] ] }

emptyFragment :: Fragment ParsedTerm
emptyFragment = termFragment (compose [])

defList :: [Def a] -> HashMap Name (Def a)
defList = H.fromList . map (defName &&& id)

testTerm :: Text -> Fragment ParsedTerm -> Spec
testTerm source expected = it (show source)
  $ case parsed source of
    Left message -> assertFailure . T.unpack $ toText [message]
    Right actual
      | expected == actual -> noop
      | otherwise -> expectedButGot
        (show expected) (show actual)

testTermFailure :: Text -> Spec
testTermFailure source = it ("should fail: " ++ show source)
  $ case parsed source of
    Left _ -> noop
    Right actual -> assertFailure $ show actual

parsed :: Text -> Either ErrorGroup (Fragment ParsedTerm)
parsed = mapLeft parseError . tokenize 1 "test"
  >=> parse "test" >=> liftM fst . rewriteInfix emptyProgram

def :: Name -> a -> Def a
def name term = Def
  { defAnno = TestAnno
  , defFixity = Postfix
  , defLocation = TestLocation
  , defName = name
  , defTerm = mono term
  }

defWithAnno :: Name -> AnType -> a -> Def a
defWithAnno name anno term = (def name term)
  { defAnno = Anno anno }

call :: Name -> ParsedTerm
call name = TrCall Postfix name TestLocation

compose :: [ParsedTerm] -> ParsedTerm
compose terms = TrCompose StackAny (V.fromList terms) TestLocation

quotation :: [ParsedTerm] -> ParsedValue
quotation terms = TrQuotation
  (TrCompose StackAny (V.fromList terms) TestLocation) TestLocation

int :: Int -> ParsedValue
int value = TrInt value TestLocation

pushi :: Int -> ParsedTerm
pushi value = push $ int value

lambda :: Name -> [ParsedTerm] -> ParsedTerm
lambda name terms
  = TrLambda name TestLocation (compose terms) TestLocation

push :: ParsedValue -> ParsedTerm
push value = TrPush value TestLocation

word :: Name -> ParsedTerm
word value = TrCall Postfix value TestLocation
