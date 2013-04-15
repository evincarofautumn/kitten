module Kitten.Error
  ( CompileError(..)
  , liftParseError
  ) where

import Text.Parsec.Error

import Kitten.Util.Either

data CompileError = CompileError String

instance Show CompileError where
  show (CompileError message) = message

liftParseError :: Either ParseError a -> Either CompileError a
liftParseError = mapLeft $ CompileError . show
