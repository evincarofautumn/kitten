module Kitten.Error
  ( CompileError(..)
  , liftParseError
  ) where

import Text.Parsec.Error

import Kitten.Util.Either

data CompileError
  = CompileError String
  | InternalError String

instance Show CompileError where
  show (CompileError message) = "compile error: " ++ message
  show (InternalError message) = "internal error: " ++ message

liftParseError :: Either ParseError a -> Either CompileError a
liftParseError = mapLeft $ CompileError . show
