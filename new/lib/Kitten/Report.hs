{-# LANGUAGE OverloadedStrings #-}

module Kitten.Report
  ( Category(..)
  , NameCategory(..)
  , Report(..)
  , human
  , parseError
  ) where

import Control.Monad.Trans.State (evalState, state)
import Data.Function (on)
import Data.List (intersperse, nub)
import Kitten.Name (GeneralName, Qualified)
import Kitten.Origin (Origin)
import Kitten.Term (Term)
import Kitten.Type (Constructor, Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Kitten.Origin as Origin
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Term as Term
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
import qualified Text.PrettyPrint as Pretty

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

instance Pretty NameCategory where
  pPrint category = case category of
    WordName -> "word"
    TypeName -> "type"

data Category = Note | Warning | Error | InternalError
  deriving (Eq)

data Report
  = MissingTypeSignature !Origin !Qualified
  | MultiplePermissionVariables !Origin !Type !Type
  | CannotResolveType !Origin !GeneralName
  | FailedInstanceCheck !Type !Type
  | MissingPermissionLabel !Type !Type !Origin !Constructor
  | TypeArgumentCountMismatch !(Term Type) [Type]
  | CannotResolveName !Origin !NameCategory !GeneralName
  | MultipleDefinitions !Origin !Qualified [Origin]
  | TypeMismatch !Type !Type
  | Chain [Report]
  | OccursCheckFailure !Type !Type
  | StackDepthMismatch !Origin
  | ParseError !Origin [Pretty.Doc] Pretty.Doc
  | Context [Pretty.Doc] Report
  deriving (Eq, Show)

human :: Report -> Pretty.Doc
human report = case report of

  MissingTypeSignature origin name -> Pretty.hsep
    [ showOriginPrefix origin
    , "I can't find a type signature for the word"
    , Pretty.quote name
    ]

  MultiplePermissionVariables origin a b -> Pretty.hcat
    [ showOriginPrefix origin
    , "I found multiple permission variables: "
    , Pretty.quote a, " and ", Pretty.quote b
    , "but only one is allowed per function"
    ]

  CannotResolveType origin name -> Pretty.hsep
    [ showOriginPrefix origin
    , "I can't tell which type", Pretty.quote name
    , "refers to (did you mean to add it as a type parameter?)"
    ]

  FailedInstanceCheck a b -> Pretty.hsep
    -- TODO: Show type kind.
    [ "the type", Pretty.quote a
    , "is not an instance of the type", Pretty.quote b
    ]

  MissingPermissionLabel a b origin name -> Pretty.hsep
    [ showOriginPrefix origin
    , "the permission label", Pretty.quote name
    , "was missing when I tried to match the permission type", Pretty.quote a
    , "with the permission type", Pretty.quote b
    ]

  TypeArgumentCountMismatch term args -> Pretty.hsep
    [ showOriginPrefix $ Term.origin term
    , "I expected", Pretty.int $ Term.quantifierCount term
    , "type arguments to", Pretty.quote term
    , "but", Pretty.int (length args), "were provided:"
    , Pretty.oxford "and" $ map Pretty.quote args
    ]

  CannotResolveName origin category name -> Pretty.hsep
    -- TODO: Suggest similar names in scope.
    [ showOriginPrefix origin
    , "I can't find the", qualification, "that the", pPrint category
    , "name", Pretty.quote name, "refers to"
    ]
    where

    qualification :: Pretty.Doc
    qualification = case category of
      WordName -> "word"
      TypeName -> "type"

  MultipleDefinitions origin name duplicates -> Pretty.hcat
    [ showOriginPrefix origin
    , "I found multiple definitions of ", Pretty.quote name
    , "(did you mean to declare it as a trait?)"
    ]

  TypeMismatch a b -> Pretty.hsep
    -- TODO: Report type kind.
    [ "I can't match the type", Pretty.quote a
    , "with the type", Pretty.quote b
    ]

  Chain reports -> Pretty.hcat $ intersperse ", because " $ map human reports

  OccursCheckFailure a b -> Pretty.hsep
    [ "the type", Pretty.quote a
    , "occurs in the type", Pretty.quote b
    , "(which often indicates an infinite type)"
    ]

  StackDepthMismatch origin -> Pretty.hsep
    [ showOriginPrefix origin
    , "there may be a stack depth mismatch"
    ]

  ParseError origin unexpected expected -> Pretty.hcat
    $ (showOriginPrefix origin :) $ intersperse "; " $ unexpected ++ [expected]

  -- TODO: Show context.
  Context context message -> human message

parseError :: Parsec.ParseError -> Report
parseError parsecError = ParseError origin unexpected' expected'
  where

  origin :: Origin
  origin = Origin.pos $ Parsec.errorPos parsecError

  sysUnexpected, unexpected, expected :: [Parsec.Message]
  (sysUnexpected, unexpected, expected)
    = flip evalState (Parsec.errorMessages parsecError) $ (,,)
      <$> state (span (Parsec.SysUnExpect "" ==))
      <*> state (span (Parsec.UnExpect "" ==))
      <*> state (span (Parsec.Expect "" ==))

  unexpected' :: [Pretty.Doc]
  unexpected' = ((++) `on` unexpectedMessages) sysUnexpected unexpected

  expected' :: Pretty.Doc
  expected' = Pretty.hsep
    [ "expected"
    , Pretty.oxford "or" $ map Pretty.text $ nub
      $ filter (not . null)  -- TODO: Replace with "end of input"
      $ map Parsec.messageString expected
    ]

  unexpectedMessages :: [Parsec.Message] -> [Pretty.Doc]
  unexpectedMessages = nub . map unexpectedMessage

  unexpectedMessage :: Parsec.Message -> Pretty.Doc
  unexpectedMessage message = let
    string = Parsec.messageString message
    in Pretty.hsep
      [ "unexpected"
      , if null string then "end of input"
        else Pretty.quotes $ Pretty.text string
      ]

showOriginPrefix :: Origin -> Pretty.Doc
showOriginPrefix origin = Pretty.hcat $
  [ Pretty.text $ Text.unpack $ Origin.name origin
  , ":", pPrint al, ".", pPrint ac, "-"
  ]
  ++ (if al == bl then [pPrint bc] else [pPrint bl, ".", pPrint bc])
  ++ [": "]
  where
  al = Origin.beginLine origin
  bl = Origin.endLine origin
  ac = Origin.beginColumn origin
  bc = Origin.endColumn origin

categoryPrefix :: Category -> Pretty.Doc
categoryPrefix category = case category of
  Note -> "note: "
  Warning -> "warning: "
  Error -> "error: "
  InternalError -> "internal error: "
