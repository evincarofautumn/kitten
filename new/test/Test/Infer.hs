{-# LANGUAGE OverloadedStrings #-}

module Test.Infer
  ( spec
  ) where

import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Informer (checkpoint)
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Kind (Kind(..))
import Kitten.Monad (runKitten)
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Type (Type(..), TypeId(..), Var(..))
import Test.Common
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do

  describe "with trivial programs" $ do

    it "typechecks empty program" $ do

      testTypecheck Positive
        "define test (->) {}"
        $ Type.fun o r r e

    it "typechecks single literals" $ do

      testTypecheck Positive
        "define test (-> Int32) { 0 }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define test (-> Float64) { 1.0 }"
        $ Type.fun o r (Type.prod o r float) e

    it "typechecks intrinsics" $ do

      testTypecheck Positive
        "define test <R..., S..., +P> (R... -> S... +P) { _::kitten::magic }"
        $ Type.fun o r s e

      testTypecheck Positive
        "define test (-> Int32) { 1 2 _::kitten::add_int }"
        $ Type.fun o r (Type.prod o r int) e

    it "typechecks data types" $ do

      testTypecheck Positive
        "type Unit { case unit }\n\
        \define test (-> Unit) { unit }"
        $ Type.fun o r (Type.prod o r (ctor "Unit")) e

      testTypecheck Positive
        "type Unit { case unit () }\n\
        \define test (-> Unit) { unit }"
        $ Type.fun o r (Type.prod o r (ctor "Unit")) e

    it "typechecks definitions" $ do

      testTypecheck Positive
        "define one (-> Int32) { 1 }\n\
        \define test (-> Int32) { one }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define one (-> Int32) { 1 }\n\
        \define two (-> Int32) { 2 }\n\
        \define test (-> Int32) { one two _::kitten::add_int }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define up (Int32 -> Int32) { 1 _::kitten::add_int }\n\
        \define down (Int32 -> Int32) { -1 _::kitten::add_int }\n\
        \define test (-> Int32) { 1 up 2 down _::kitten::add_int }"
        $ Type.fun o r (Type.prod o r int) e

    it "typechecks operators" $ do

      testTypecheck Positive
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \define test (-> Int32) { 1 + 1 }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    right 5\n\
        \define test (-> Int32) { 1 + 1 }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    right\n\
        \define test (-> Int32) { 1 + 1 }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Positive
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    5\n\
        \define test (-> Int32) { 1 + 1 }"
        $ Type.fun o r (Type.prod o r int) e

    it "typechecks nested scopes" $ do

      testTypecheck Positive
        "intrinsic add (Int32, Int32 -> Int32)\n\
        \define test (-> Int32, Int32) {\n\
        \  1000 -> x1;\n\
        \  100 -> y1;\n\
        \  10\n\
        \  {\n\
        \    -> a1;\n\
        \    a1 x1 add\n\
        \    {\n\
        \      -> b1;\n\
        \      b1 y1 add\n\
        \    } call\n\
        \  } call\n\
        \  \n\
        \  1000 -> x2;\n\
        \  100 -> y2;\n\
        \  10\n\
        \  {\n\
        \    -> a2;\n\
        \    a2 y2 add\n\
        \    {\n\
        \      -> b2;\n\
        \      b2 x2 add\n\
        \    } call\n\
        \  } call\n\
        \}"
        $ Type.fun o r (Type.prod o (Type.prod o r int) int) e

    it "typechecks closures with multiple types" $ do
      testTypecheck Positive
        "define test (-> (-> Int32, Float64)) {\n\
        \  0 0.0 -> x, y;\n\
        \  { x y }\n\
        \}"
        $ Type.fun o r (Type.prod o r
          (Type.fun o r (Type.prod o (Type.prod o r int) float) e)) e

  describe "with instance checking" $ do

    it "rejects invalid signature" $ do

      testTypecheck Negative
        "type Pair<A, B> { case pair (A, B) }\n\
        \define flip<A, B> (Pair<A, B> -> Pair<A, B>) {\n\
        \  match case pair -> x, y { y x pair }\n\
        \}\n\
        \define test (-> Pair<Char, Int32>) { 1 '1' pair flip }"
        $ Type.fun o r (Type.prod o r (ctor "Pair" :@ char :@ int)) e

    it "accepts valid permissions" $ do

      testTypecheck Positive
        "define test (-> +Fail) { abort }"
        $ Type.fun o r r (Type.join o fail_ e)

      testTypecheck Positive
        "intrinsic launch_missiles (-> +IO)\n\
        \define test (-> +Fail +IO) { launch_missiles abort }"
        $ Type.fun o r r (Type.join o fail_ (Type.join o io e))

      testTypecheck Positive
        "intrinsic launch_missiles (-> +IO)\n\
        \define test (-> +IO +Fail) { launch_missiles abort }"
        $ Type.fun o r r (Type.join o fail_ (Type.join o io e))

    it "accepts redundant permissions" $ do

      testTypecheck Positive
        "define test (-> +Fail) {}"
        $ Type.fun o r r (Type.join o fail_ e)

      testTypecheck Positive
        "define test (-> +Fail +IO) {}"
        $ Type.fun o r r (Type.join o fail_ (Type.join o io e))

      testTypecheck Positive
        "define test (-> +IO +Fail) {}"
        $ Type.fun o r r (Type.join o fail_ (Type.join o io e))

    it "rejects missing permissions" $ do

      testTypecheck Negative
        "define test (->) { abort }"
        $ Type.fun o r r e

      testTypecheck Negative
        "intrinsic launch_missiles (-> +IO)\n\
        \define test (->) { launch_missiles abort }"
        $ Type.fun o r r e

  describe "with higher-order functions" $ do

    it "typechecks curried functions" $ do

      testTypecheck Positive
        "define curried_add (Int32 -> Int32 -> Int32) {\n\
        \  -> x; { -> y; x y _::kitten::add_int }\n\
        \}\n\
        \define test (-> Int32) { 1 2 curried_add call }"
        $ Type.fun o r (Type.prod o r int) e

    it "typechecks permissions of higher-order functions" $ do

      testTypecheck Positive
        "intrinsic launch_missiles (-> +IO)\n\
        \intrinsic map<A, B, +P> (List<A>, (A -> B +P) -> List<B> +P)\n\
        \define test (-> List<Int32> +IO) { [1, 2, 3] \\launch_missiles map }"
        $ Type.fun o r (Type.prod o r (ctor "List" :@ int)) (Type.join o io e)

  describe "with coercions" $ do

    it "typechecks identity coercions" $ do

      testTypecheck Positive
        "define test (-> Int32) { 1i32 as (Int32) }"
        $ Type.fun o r (Type.prod o r int) e

      testTypecheck Negative
        "define test (-> Int32) { 1i64 as (Int32) }"
        $ Type.fun o r (Type.prod o r int) e

  describe "with named fields" $ do

    it "reads value from field" $ do

      testTypecheck Positive
        "type Example {\n\
        \  case example {\n\
        \    value as (Int32)\n\
        \  }\n\
        \}\n\
        \define test (-> Int32) {\n\
        \  (42 example).value\n\
        \}"
        $ Type.fun o r (Type.prod o r int) e

  where
  o = Origin.point "" 0 0
  r = TypeVar o $ Var (TypeId 0) Stack
  s = TypeVar o $ Var (TypeId 1) Stack
  e = TypeVar o $ Var (TypeId 2) Permission
  ctor = TypeConstructor o . Type.Constructor
    . Qualified Vocabulary.global
  char = ctor "Char"
  int = ctor "Int32"
  io = ctor "IO"
  fail_ = ctor "Fail"
  float = ctor "Float64"

testTypecheck :: Sign -> Text -> Type -> IO ()
testTypecheck sign input expected = do
  result <- runKitten $ do
    let io = [QualifiedName $ Qualified Vocabulary.global "IO"]
    fragment <- fragmentFromSource io Nothing 1 "<test>" input
    -- FIXME: Avoid redundantly reparsing common vocabulary.
    common <- fragmentFromSource io Nothing 1 "<common>" commonSource
    commonDictionary <- Enter.fragment common Dictionary.empty
    Enter.fragment fragment commonDictionary
  case Dictionary.toList <$> result of
    Right definitions -> case find matching definitions of
      Just (_, Entry.Word _ _ _ _ _ (Just term)) -> do
        let
          actual = Term.type_ term
        check <- runKitten $ do
          instanceCheck "inferred" actual "declared" expected
          checkpoint
        case sign of
          Positive -> assertBool
            (Pretty.render $ Pretty.hsep [pPrint actual, "<:", pPrint expected])
            $ either (const False) (const True) check
          Negative -> assertBool
            (Pretty.render $ Pretty.hsep [pPrint actual, "</:", pPrint expected])
            $ either (const True) (const False) check
      _ -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing main word definition:", pPrint definitions]
      where
      matching (Instantiated (Qualified v "test") _, _)
        | v == Vocabulary.global
        = True
      matching _ = False
    Left reports -> case sign of
      Positive -> assertFailure $ unlines
        $ map (Pretty.render . Report.human) reports
      -- FIXME: This might accept a negative test for the wrong
      -- reason.
      Negative -> return ()

-- FIXME: Avoid redundantly re-parsing common vocabulary.
commonSource :: Text
commonSource = "\
\vocab kitten {\
\  intrinsic call<R..., S...> (R..., (R... -> S...) -> S...)\n\
\  intrinsic magic<R..., S...> (R... -> S...)\n\
\  intrinsic add_int (_::Int32, _::Int32 -> _::Int32)\n\
\}\n\
\define call<R..., S...> (R..., (R... -> S...) -> S...) {\n\
\  _::kitten::call\n\
\}\n\
\intrinsic abort<R..., S...> (R... -> S... +Fail)\n\
\type Char {}\n\
\type Float64 {}\n\
\type Int32 {}\n\
\type List<T> {}\n\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E) {\n\
\  with (+IO)\n\
\}\n\
\permission Fail<R..., S..., +E> (R..., (R... -> S... +Fail +E) -> S... +E) {\n\
\  with (+Fail)\n\
\}\n\
\\&"
