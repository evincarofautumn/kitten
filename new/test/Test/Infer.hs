{-# LANGUAGE OverloadedStrings #-}

module Test.Infer
  ( spec
  ) where

import Data.List (find)
import Data.Text (Text)
import Kitten.Infer (inferTypes)
import Kitten.Informer (checkpoint)
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Kind (Kind(..))
import Kitten.Layout (layout)
import Kitten.Monad (runKitten)
import Kitten.Name (Qualified(..))
import Kitten.Parse (parse)
import Kitten.Program (Program)
import Kitten.Report (Report)
import Kitten.Resolve (resolveNames)
import Kitten.Scope (scope)
import Kitten.Tokenize (tokenize)
import Kitten.Type (Type(..), TypeId(..), Var(..))
import Kitten.Vocabulary (globalVocabulary)
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Kitten.Desugar.Data as Data
import qualified Kitten.Desugar.Infix as Infix
import qualified Kitten.Origin as Origin
import qualified Kitten.Program as Program
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  describe "with trivial programs" $ do
    it "typechecks empty program" $ do
      testTypecheck "" $ Type.funType o r r e
    it "typechecks single literals" $ do
      testTypecheck "0" $ Type.funType o r (Type.prodType o r int) e
      testTypecheck "1.0" $ Type.funType o r (Type.prodType o r float) e
  where
  o = Origin.point "" 0 0
  r = TypeVar o $ Var (TypeId 0) Stack
  _s = TypeVar o $ Var (TypeId 1) Stack
  e = TypeVar o $ Var (TypeId 2) Permission
  int = TypeConstructor o $ Type.Constructor
    $ Qualified globalVocabulary "int"
  float = TypeConstructor o $ Type.Constructor
    $ Qualified globalVocabulary "float"

testTypecheck :: Text -> Type -> IO ()
testTypecheck input expected = do
  result <- typecheck input
  case HashMap.toList . Program.definitions <$> result of
    Right definitions -> case find matching definitions of
      Just (_, term) -> do
        let
          actual = Term.type_ term
        check <- runKitten
          $ instanceCheck "inferred" actual "declared" expected
        assertBool
          (Pretty.render
            $ Pretty.hsep [pPrint actual, "unifies with", pPrint expected])
          $ either (const False) (const True) check
      Nothing -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing main definition:", pPrint definitions]
      where
      matching ((Qualified v "main", _), _) | v == globalVocabulary = True
      matching _ = False
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports

-- FIXME: Avoid redundancy with 'Kitten.compile'.
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
  inferTypes scoped
  where
  -- FIXME: Avoid redundantly re-parsing common vocabulary.
  common = "\
\type float {}\n\
\type int {}\n\
\permission io<R..., S..., +E> (R..., (R... -> S... +io +E) -> S... +E) { _::magic }\n"
