{-# LANGUAGE OverloadedStrings #-}

module Error (Error(..), Error.Monad) where

import qualified Text

import qualified Control.Monad.Error as CME
import Data.List
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

data Error
  = CompileError Text.Text
  | InternalError Text.Text
  | ParseError Parsec.ParseError

type Monad = Either Error

instance Show Error where
  show (CompileError message) = Text.unpack message
  show (InternalError message)
    = if Text.null message
        then "Internal error."
        else "Internal error: " ++ show message
  show (ParseError message)
    = concatMap
      (\ x -> intercalate ":" locations ++ ": " ++ x ++ "\n") messages
    where
      pos = Parsec.errorPos message
      messages = map showMessage (Parsec.errorMessages message)
      showMessage (Parsec.SysUnExpect s) = "Unexpected " ++ s ++ "."
      showMessage (Parsec.UnExpect s) = "Unexpected " ++ s ++ "."
      showMessage (Parsec.Expect s) = "Expected " ++ s ++ "."
      showMessage (Parsec.Message s) = s ++ "."
      locations
        = [ Parsec.sourceName pos
          , show (Parsec.sourceLine pos)
          , show (Parsec.sourceColumn pos)
          ]

instance CME.Error Error where
  strMsg = InternalError . Text.pack
  noMsg = InternalError ""
