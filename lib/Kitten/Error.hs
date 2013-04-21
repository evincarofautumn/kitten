module Kitten.Error
  ( CompileError(..)
  , liftParseError
  ) where

import Text.Parsec.Error

import Kitten.Util.Either

data CompileError
  = CompileError String
  | InternalError String
  | TypeError String

instance Show CompileError where
  show compileError = case compileError of
    CompileError message -> "compile error: " ++ message
    InternalError message -> "internal error: " ++ message
    TypeError message -> "type error: " ++ message

liftParseError :: Either ParseError a -> Either CompileError a
liftParseError = mapLeft $ CompileError . show
