{-# LANGUAGE OverloadedStrings #-}

module Error
  ( Error(..)
  , ErrorMonad
  , impossible
  , CME.throwError
  ) where

import qualified Text

import qualified Control.Monad.Error as CME
import Data.List
import FileLocation
import Language.Haskell.TH.Syntax
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

data Error
  = CompileError Text.Text
  | InternalError Text.Text
  | ParseError Parsec.ParseError

type ErrorMonad = Either Error

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

impossible :: Q Exp
impossible = err "The impossible has happened."
