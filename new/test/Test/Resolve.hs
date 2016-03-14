{-# LANGUAGE OverloadedStrings #-}

module Test.Resolve
  ( spec
  ) where

import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Monad (runKitten)
import Kitten.Name (GeneralName(..), Qualified)
import Kitten.Name (Qualified(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  it "resolves global names" $ do
    testResolve
      "define f (->) {}"
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")

testResolve :: Text -> GeneralName -> Qualified -> IO ()
testResolve contextSource name expected = do
  dictionary <- runKitten $ do
    context <- fragmentFromSource [] "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let
      origin = Origin.point "<test>" 0 0
      fragment = mempty
        { Fragment.definitions = [Definition.main []
          (Term.Word () Operator.Postfix name [] origin)]
        }
    Enter.fragment fragment contextDictionary
  case Dictionary.toList <$> dictionary of
    Right definitions -> case find matching definitions of
      Just (_, Entry.Word _ _ _ _ _ (Just term))
        | [Term.Word _ _ name' _ _] <- Term.decompose term -> let
        message = Pretty.render $ Pretty.hsep
          [pPrint name, "resolves to", pPrint expected]
        in assertEqual message name' (QualifiedName expected)
      _ -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing main word definition:", pPrint definitions]
      where
      matching (Qualified v "main", _) | v == Vocabulary.global = True
      matching _ = False
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports
