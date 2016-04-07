{-# LANGUAGE OverloadedStrings #-}

module Test.Resolve
  ( spec
  ) where

import Data.List (find)
import Data.Text (Text)
import Kitten (fragmentFromSource)
import Kitten.Definition (Definition(Definition))
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Instantiated (Instantiated(Instantiated))
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
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do

  it "resolves global words" $ do
    testWord
      "define f (->) {}"
      Vocabulary.global
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testWord
      "define f (->) {}"
      (Qualifier ["v"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testWord
      "define f (->) {}"
      (Qualifier ["v1", "v2"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")

  it "resolves words in the same vocabulary" $ do
    testWord
      "vocab v { define f (->) {} }"
      (Qualifier [Vocabulary.globalName, "v"])
      (UnqualifiedName "f")
      (Qualified (Qualifier [Vocabulary.globalName, "v"]) "f")
    testWord
      "vocab v1 { vocab v2 { define f (->) {} } }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (UnqualifiedName "f")
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "f")
    testWord
      "vocab v1::v2 { define f (->) {} }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (UnqualifiedName "f")
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "f")

  it "resolves words in nested vocabularies" $ do
    testWord
      "vocab v1::v2 { define f (->) {} }"
      (Qualifier [Vocabulary.globalName, "v1"])
      (QualifiedName (Qualified (Qualifier ["v2"]) "f"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "f")
    testWord
      "vocab v1::v2::v3 { define f (->) {} }"
      (Qualifier [Vocabulary.globalName, "v1"])
      (QualifiedName (Qualified (Qualifier ["v2", "v3"]) "f"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2", "v3"]) "f")
    testWord
      "vocab v1::v2::v3 { define f (->) {} }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (QualifiedName (Qualified (Qualifier ["v3"]) "f"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2", "v3"]) "f")

  it "resolves global types" $ do
    testType
      "type T {}"
      Vocabulary.global
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")
    testType
      "type T {}"
      (Qualifier ["v"])
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")
    testType
      "type T {}"
      (Qualifier ["v1", "v2"])
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")

  it "resolves types in the same vocabulary" $ do
    testType
      "vocab v { type T {} }"
      (Qualifier [Vocabulary.globalName, "v"])
      (UnqualifiedName "T")
      (Qualified (Qualifier [Vocabulary.globalName, "v"]) "T")
    testType
      "vocab v1 { vocab v2 { type T {} } }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (UnqualifiedName "T")
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "T")
    testType
      "vocab v1::v2 { type T {} }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (UnqualifiedName "T")
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "T")

  it "resolves types in nested vocabularies" $ do
    testType
      "vocab v1::v2 { type T {} }"
      (Qualifier [Vocabulary.globalName, "v1"])
      (QualifiedName (Qualified (Qualifier ["v2"]) "T"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2"]) "T")
    testType
      "vocab v1::v2::v3 { type T {} }"
      (Qualifier [Vocabulary.globalName, "v1"])
      (QualifiedName (Qualified (Qualifier ["v2", "v3"]) "T"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2", "v3"]) "T")
    testType
      "vocab v1::v2::v3 { type T {} }"
      (Qualifier [Vocabulary.globalName, "v1", "v2"])
      (QualifiedName (Qualified (Qualifier ["v3"]) "T"))
      (Qualified (Qualifier [Vocabulary.globalName, "v1", "v2", "v3"]) "T")

  it "resolves types in trait signatures" $ do
    testType
      "type Size {}\n\
      \trait alignment<T> (T -> Size)"
      Vocabulary.global
      (UnqualifiedName "Size")
      (Qualified Vocabulary.global "Size")

testWord :: Text -> Qualifier -> GeneralName -> Qualified -> IO ()
testWord contextSource viewpoint name expected = do
  dictionary <- runKitten $ do
    context <- fragmentFromSource [] Nothing 1 "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let
      origin = Origin.point "<test>" 0 0
      fragment = mempty
        { Fragment.definitions = (:[]) Definition
          { Definition.body = Term.Word () Operator.Postfix name [] origin
          , Definition.category = Category.Word
          , Definition.fixity = Operator.Postfix
          , Definition.inferSignature = False
          , Definition.merge = Merge.Deny
          , Definition.name = Qualified viewpoint "test"
          , Definition.origin = origin
          , Definition.parent = Nothing
          , Definition.signature = Signature.Quantified
            [Parameter origin "R" Stack, Parameter origin "S" Stack]
            (Signature.StackFunction
              (Signature.Variable "R" origin) []
              (Signature.Variable "S" origin) []
              [] origin) origin
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
        in assertEqual message (QualifiedName expected) name'
      _ -> assertFailure $ Pretty.render $ Pretty.hsep
        ["missing test word definition:", pPrint definitions]
      where
      matching (Instantiated (Qualified v "test") _, _)
        | v == viewpoint
        = True
      matching _ = False
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports

testType :: Text -> Qualifier -> GeneralName -> Qualified -> IO ()
testType contextSource viewpoint name expected = do
  resolved <- runKitten $ do
    context <- fragmentFromSource [] Nothing 1 "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let origin = Origin.point "<test>" 0 0
    Resolve.run $ Resolve.signature contextDictionary viewpoint
      (Signature.Variable name origin)
  case resolved of
    Right (Signature.Variable name' _) -> let
      message = Pretty.render $ Pretty.hsep
        [ pPrint name
        , "resolves to"
        , pPrint expected
        , "within"
        , pPrint viewpoint
        ]
      in assertEqual message (QualifiedName expected) name'
    Right result -> assertFailure $ Pretty.render $ Pretty.hsep
      [ "signature variable"
      , Pretty.quote name
      , "resolved to non-variable"
      , pPrint result
      ]
    Left reports -> assertFailure $ unlines
      $ map (Pretty.render . Report.human) reports
