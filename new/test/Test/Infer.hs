{-# LANGUAGE OverloadedStrings #-}

module Test.Infer
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (typecheck)
import Kitten.Informer (checkpoint)
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Kind (Kind(..))
import Kitten.Layout (layout)
import Kitten.Monad (runKitten)
import Kitten.Name (Qualified(..))
import Kitten.Parse (parse)
import Kitten.Report (Report)
import Kitten.Scope (scope)
import Kitten.Tokenize (tokenize)
import Kitten.Type (Type(..), TypeId(..), Var(..))
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Kitten.Desugar.Data as Data
import qualified Kitten.Desugar.Infix as Infix
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
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
{-
    it "typechecks data types" $ do
      testTypecheck "type Unit { case unit } unit"
        $ Type.funType o r (Type.prodType o r (ctor "Unit")) e
      testTypecheck "type Unit { case unit () } unit"
        $ Type.funType o r (Type.prodType o r (ctor "Unit")) e
      testTypecheck "type Unit () unit"
        $ Type.funType o r (Type.prodType o r (ctor "Unit")) e
-}
  where
  o = Origin.point "" 0 0
  r = TypeVar o $ Var (TypeId 0) Stack
  s = TypeVar o $ Var (TypeId 1) Stack
  e = TypeVar o $ Var (TypeId 2) Permission
  ctor = TypeConstructor o . Type.Constructor
    . Qualified Vocabulary.global
  int = ctor "Int"
  float = ctor "Float"

testTypecheck :: Text -> Type -> IO ()
testTypecheck input expected = do
  result <- runKitten $ do
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Input:", Pretty.text $ show input]
    fragment <- fragmentFromSource "<test>" input
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Parsed:", pPrint fragment]
    -- FIXME: Avoid redundantly reparsing common vocabulary.
    common <- fragmentFromSource "<common>" commonSource
    liftIO $ putStrLn $ Pretty.render $ Pretty.hsep ["Common frag:", pPrint common]
    commonDictionary <- Enter.fragment common Dictionary.empty
    Enter.fragment fragment commonDictionary
  case HashMap.toList . Dictionary.entries <$> result of
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
\  intrinsic add_int (_::Int, _::Int -> _::Int)\
\}\
\type Float {}\n\
\type Int {}\n\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E) {\n\
\  with (+IO)\n\
\}\n"
