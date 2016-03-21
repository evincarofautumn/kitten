{-# LANGUAGE OverloadedStrings #-}

module Test.Infer
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Informer (checkpoint)
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Kind (Kind(..))
import Kitten.Monad (runKitten)
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Type (Type(..), TypeId(..), Var(..))
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
      testTypecheck "" $ Type.funType o r r e
    it "typechecks single literals" $ do
      testTypecheck "0" $ Type.funType o r (Type.prodType o r int) e
      testTypecheck "1.0" $ Type.funType o r (Type.prodType o r float) e
    it "typechecks intrinsics" $ do
      testTypecheck "_::kitten::magic" $ Type.funType o r s e
      testTypecheck "1 2 _::kitten::add_int"
        $ Type.funType o r (Type.prodType o r int) e
    it "typechecks data types" $ do
      testTypecheck "type Unit { case unit } unit"
        $ Type.funType o r (Type.prodType o r (ctor "Unit")) e
      testTypecheck "type Unit { case unit () } unit"
        $ Type.funType o r (Type.prodType o r (ctor "Unit")) e
    it "typechecks definitions" $ do
      testTypecheck "define one (-> Int32) { 1 } one"
        $ Type.funType o r (Type.prodType o r int) e
      testTypecheck
        "define one (-> Int32) { 1 }\n\
        \define two (-> Int32) { 2 }\n\
        \one two _::kitten::add_int"
        $ Type.funType o r (Type.prodType o r int) e
      testTypecheck
        "define up (Int32 -> Int32) { 1 _::kitten::add_int }\n\
        \define down (Int32 -> Int32) { -1 _::kitten::add_int }\n\
        \1 up 2 down _::kitten::add_int"
        $ Type.funType o r (Type.prodType o r int) e
    it "typechecks operators" $ do
      testTypecheck
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \1 + 1"
        $ Type.funType o r (Type.prodType o r int) e
      testTypecheck
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    right 5\n\
        \1 + 1"
        $ Type.funType o r (Type.prodType o r int) e
      testTypecheck
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    right\n\
        \1 + 1"
        $ Type.funType o r (Type.prodType o r int) e
      testTypecheck
        "define + (Int32, Int32 -> Int32) { _::kitten::add_int }\n\
        \about +:\n\
        \  operator:\n\
        \    5\n\
        \1 + 1"
        $ Type.funType o r (Type.prodType o r int) e

    it "typechecks nested scopes" $ do
      testTypecheck
        "intrinsic add (Int32, Int32 -> Int32)\n\
        \1000 -> x1;\n\
        \100 -> y1;\n\
        \10\n\
        \{\n\
        \  -> a1;\n\
        \  a1 x1 add\n\
        \  {\n\
        \    -> b1;\n\
        \    b1 y1 add\n\
        \  } call\n\
        \} call\n\
        \\n\
        \1000 -> x2;\n\
        \100 -> y2;\n\
        \10\n\
        \{\n\
        \  -> a2;\n\
        \  a2 y2 add\n\
        \  {\n\
        \    -> b2;\n\
        \    b2 x2 add\n\
        \  } call\n\
        \} call\n\
        \\&"
        $ Type.funType o r (Type.prodType o (Type.prodType o r int) int) e

  where
  o = Origin.point "" 0 0
  r = TypeVar o $ Var (TypeId 0) Stack
  s = TypeVar o $ Var (TypeId 1) Stack
  e = TypeVar o $ Var (TypeId 2) Permission
  ctor = TypeConstructor o . Type.Constructor
    . Qualified Vocabulary.global
  int = ctor "Int32"
  float = ctor "Float64"

testTypecheck :: Text -> Type -> IO ()
testTypecheck input expected = do
  result <- runKitten $ do
    let io = [QualifiedName $ Qualified Vocabulary.global "IO"]
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Input:", Pretty.text $ show input]
    fragment <- fragmentFromSource io Nothing 1 "<test>" input
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Parsed:", pPrint fragment]
    -- FIXME: Avoid redundantly reparsing common vocabulary.
    common <- fragmentFromSource io Nothing 1 "<common>" commonSource
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Common frag:", pPrint common]
    commonDictionary <- Enter.fragment common Dictionary.empty
    Enter.fragment fragment commonDictionary
  case Dictionary.toList <$> result of
    Right definitions -> case find matching definitions of
      Just (_, Entry.Word _ _ _ _ _ (Just term)) -> do
        let
          actual = Term.type_ term
        check <- runKitten $ do
          instanceCheck "declared" expected "inferred" actual
          checkpoint
        assertBool
          (Pretty.render
            $ Pretty.hsep [pPrint actual, "unifies with", pPrint expected])
          $ either (const False) (const True) check
      _ -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing main word definition:", pPrint definitions]
      where
      matching (Qualified v "main", _) | v == Vocabulary.global = True
      matching _ = False
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports

-- FIXME: Avoid redundancy with 'Kitten.compile'.
{-
typecheck :: Text -> IO (Either [Report] (Program Type))
typecheck input = runKitten $ do
  tokenized <- tokenize "" $ Text.concat [common, input]
  checkpoint
  laidout <- layout "" tokenized
  checkpoint
  parsed <- parse "" laidout
  checkpoint
  dataDesugared <- Data.desugar parsed
  resolved <- resolveNames dataDesugared
  checkpoint
  postfix <- Infix.desugar resolved
  checkpoint
  let scoped = scope postfix
  inferred <- inferTypes scoped
  checkpoint
  return inferred
  where
-}

-- FIXME: Avoid redundantly re-parsing common vocabulary.
commonSource :: Text
commonSource = "\
\vocab kitten {\
\  intrinsic magic<R..., S...> (R... -> S...)\
\  intrinsic add_int (_::Int32, _::Int32 -> _::Int32)\
\}\
\type Float {}\n\
\type Int32 {}\n\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E) {\n\
\  with (+IO)\n\
\}\n"
