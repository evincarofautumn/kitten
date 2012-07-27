{-# LANGUAGE OverloadedStrings #-}

module Error (Error(..), Error.Monad) where

import qualified Control.Monad.Error as CME
import Data.List
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified Text

data Error
  = CompileError Text.Text
  | InternalError Text.Text
  | ParseError Parsec.ParseError
  | TokenizeError Parsec.ParseError

type Monad = Either Error

instance Show Error where
  show (CompileError message) = show message
  show (InternalError message)
    = if Text.null message
        then "Internal error."
        else "Internal error: " ++ show message
  show (TokenizeError err) = showParsecError err
  show (ParseError err) = showParsecError err

showParsecError err = concatMap format messages
  where
    format message = intercalate ":" locations ++ ": " ++ message ++ "\n"
    messages = map showMessage (Parsec.errorMessages err)
    showMessage (Parsec.SysUnExpect s) = "Unexpected " ++ s ++ "."
    showMessage (Parsec.UnExpect s)    = "Unexpected " ++ s ++ "."
    showMessage (Parsec.Expect s)      = "Expected " ++ s ++ "."
    showMessage (Parsec.Message s)     = s ++ "."
    pos = Parsec.errorPos err
    locations
      = [ Parsec.sourceName pos
        , show $ Parsec.sourceLine pos
        , show $ Parsec.sourceColumn pos
        ]

instance CME.Error Error where
  strMsg = InternalError . Text.pack
  noMsg = InternalError ""
