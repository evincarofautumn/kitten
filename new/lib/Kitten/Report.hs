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
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import Kitten.Type (Constructor, Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Kitten.Origin as Origin
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
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
  | WordRedefinition !Origin !Qualified !Origin
  | WordRedeclaration !Origin !Qualified !Signature !Origin !Signature
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

  MultiplePermissionVariables origin a b -> Pretty.hsep
    [ showOriginPrefix origin
    , "I found multiple permission variables:"
    , Pretty.quote a, "and", Pretty.quote b
    , "but only one is allowed per function"
    ]

  CannotResolveType origin name -> Pretty.hsep
    [ showOriginPrefix origin
    , "I can't tell which type", Pretty.quote name
    , "refers to (did you mean to add it as a type parameter?)"
    ]

  FailedInstanceCheck a b -> Pretty.hsep
    -- TODO: Show type kind.
    [ "I expected"
    , Pretty.quote a
    , "to be an instance of the type"
    , Pretty.quote b
    , "but it isn't"
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

  MultipleDefinitions origin name duplicates -> Pretty.hsep
    [ showOriginPrefix origin
    , "I found multiple definitions of"
    , Pretty.quote name
    , "(did you mean to declare it as a trait?)"
    ]

  WordRedefinition origin name originalOrigin -> Pretty.hsep
    [ showOriginPrefix origin
    , "I can't redefine the word"
    , Pretty.quote name
    , "because it already exists (did you mean to declare it as a trait?)"
    ]

  WordRedeclaration origin name signature
    originalOrigin originalSignature -> Pretty.vcat
    [ Pretty.hsep
      [ showOriginPrefix origin
      , "I can't redeclare the word"
      , Pretty.quote name
      , "with the signature"
      , Pretty.quote signature
      ]
    , Pretty.hsep
      [ showOriginPrefix originalOrigin
      , "because it was declared or defined already with the signature"
      , Pretty.quote originalSignature
      ]
    ]

  -- TODO: Report type kind.
  TypeMismatch a b -> Pretty.vcat
    [ Pretty.hsep
      [ showOriginPrefix $ Type.origin a
      , "I can't match the type"
      , Pretty.quote a
      ]
    , Pretty.hsep
      [ showOriginPrefix $ Type.origin b
      , "with the type", Pretty.quote b
      ]
    ]

  Chain reports -> Pretty.vcat $ map human reports

  OccursCheckFailure a b -> Pretty.vcat
    [ Pretty.hsep
      [ showOriginPrefix $ Type.origin a
      , "the type"
      , Pretty.quote a
      ]
    , Pretty.hsep
      [ showOriginPrefix $ Type.origin b
      , "occurs in the type"
      , Pretty.quote b
      , "(which often indicates an infinite type)"
      ]
    ]

  StackDepthMismatch origin -> Pretty.hsep
    [ showOriginPrefix origin
    , "you may have a stack depth mismatch"
    ]

  ParseError origin unexpected expected -> Pretty.hcat
    $ (showOriginPrefix origin :) $ intersperse "; " $ unexpected ++ [expected]

  -- TODO: Show context.
  Context context message -> human message

showOriginPrefix :: Origin -> Pretty.Doc
showOriginPrefix origin = Pretty.hcat [pPrint origin, ":"]

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

categoryPrefix :: Category -> Pretty.Doc
categoryPrefix category = case category of
  Note -> "note: "
  Warning -> "warning: "
  Error -> "error: "
  InternalError -> "internal error: "
