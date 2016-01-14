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
import Kitten.Origin (Origin(Origin))
import Kitten.Term (Term)
import Kitten.Type (Constructor, Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
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
  | MultipleEffectVariables !Origin !Type !Type
  | CannotResolveType !Origin !GeneralName
  | FailedInstanceCheck !Type !Type
  | MissingEffectLabel !Type !Type !Origin !Constructor
  | TypeArgumentCountMismatch !Term [Type]
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
    ["I can't find a type signature for the word", Pretty.quote name]

  MultipleEffectVariables origin a b -> Pretty.hcat
    ["I found multiple effect variables: "
    , Pretty.quote a, " and ", Pretty.quote b
    , "but only one is allowed per function"
    ]

  CannotResolveType origin name -> Pretty.hsep
    [ "I can't tell which type", Pretty.quote name
    , "refers to (did you mean to add it as a type parameter?)"
    ]

  FailedInstanceCheck a b -> Pretty.hsep
    -- TODO: Show type kind.
    [ "the type", Pretty.quote a
    , "is not an instance of the type", Pretty.quote b
    ]

  MissingEffectLabel a b origin name -> Pretty.hsep
    [ "the effect label", Pretty.quote name
    , "was missing when I tried to match the effect type", Pretty.quote a
    , "with the effect type", Pretty.quote b
    ]

  TypeArgumentCountMismatch term args -> Pretty.hsep
    [ "I expected", Pretty.int $ Term.quantifierCount term
    , "type arguments to", Pretty.quote term
    , "but", Pretty.int (length args), "were provided:"
    , Pretty.oxford "and" $ map Pretty.quote args
    ]

  CannotResolveName origin category name -> Pretty.hsep
    -- TODO: Suggest similar names in scope.
    [ "I can't find the", qualification, "that the", pPrint category
    , "name", Pretty.quote name, "refers to"
    ]
    where

    qualification :: Pretty.Doc
    qualification = case category of
      WordName -> "word"
      TypeName -> "type"

  MultipleDefinitions origin name duplicates -> Pretty.hcat
    [ "I found multiple definitions of ", Pretty.quote name
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

  StackDepthMismatch origin
    -> "there may be a stack depth mismatch"

  ParseError{} -> error "TODO: show parse error"

parseError :: Parsec.ParseError -> Report
parseError parsecError = ParseError origin unexpected' expected'
  where

  origin :: Origin
  origin = Origin
    { Origin.begin = Parsec.errorPos parsecError
    , Origin.end = Parsec.errorPos parsecError
    }

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

showOriginPrefix :: Origin -> String
showOriginPrefix (Origin a b) = concat
  $ [Parsec.sourceName a, ":", show al, ".", show ac, "-"]
  ++ (if al == bl then [show bc] else [show bl, ".", show bc])
  ++ [": "]
  where
  al = Parsec.sourceLine a
  bl = Parsec.sourceLine b
  ac = Parsec.sourceColumn a
  bc = Parsec.sourceColumn b

categoryPrefix :: Category -> Pretty.Doc
categoryPrefix category = case category of
  Note -> "note: "
  Warning -> "warning: "
  Error -> "error: "
  InternalError -> "internal error: "
