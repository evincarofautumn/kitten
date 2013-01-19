module Error
  ( CompileError(..)
  , failIfError
  ) where

import Text.Parsec.Error

import Util

data CompileError
  = CompileError !String

instance Show CompileError where
  show (CompileError message) = message

failIfError :: Either ParseError a -> Either CompileError a
failIfError = mapLeft $ CompileError . show
