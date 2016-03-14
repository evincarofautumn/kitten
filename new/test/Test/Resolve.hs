{-# LANGUAGE OverloadedStrings #-}

module Test.Resolve
  ( spec
  ) where

import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Definition (Definition(Definition))
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Kind (Kind(..))
import Kitten.Monad (runKitten)
import Kitten.Name (GeneralName(..), Qualified(..), Qualifier(..))
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
import qualified Kitten.Report as Report
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  it "resolves global names" $ do
    testResolve
      "define f (->) {}"
      Vocabulary.global
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testResolve
      "define f (->) {}"
      (Qualifier ["v"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testResolve
      "define f (->) {}"
      (Qualifier ["v1", "v2"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")

testResolve :: Text -> Qualifier -> GeneralName -> Qualified -> IO ()
testResolve contextSource viewpoint name expected = do
  dictionary <- runKitten $ do
    context <- fragmentFromSource [] "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let
      origin = Origin.point "<test>" 0 0
      fragment = mempty
        { Fragment.definitions = (:[]) Definition
          { Definition.body = Term.Word () Operator.Postfix name [] origin
          , Definition.category = Category.Word
          , Definition.fixity = Operator.Postfix
          , Definition.merge = Merge.Deny
          , Definition.name = Qualified viewpoint "test"
          , Definition.origin = origin
          , Definition.signature = Signature.Quantified
            [Parameter origin "R" Stack, Parameter origin "S" Stack]
            (Signature.StackFunction "R" [] "S" [] [] origin) origin
          }
        }
    Enter.fragment fragment contextDictionary
  case Dictionary.toList <$> dictionary of
    Right definitions -> case find matching definitions of
      Just (_, Entry.Word _ _ _ _ _ (Just term))
        | [Term.Word _ _ name' _ _] <- Term.decompose term -> let
        message = Pretty.render $ Pretty.hsep
          [ pPrint name
          , "resolves to"
          , pPrint expected
          , "within"
          , pPrint viewpoint
          ]
        in assertEqual message name' (QualifiedName expected)
      _ -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing test word definition:", pPrint definitions]
      where
      matching (Qualified v "test", _) | v == viewpoint = True
      matching _ = False
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports
