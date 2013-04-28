module Kitten.Error
  ( CompileError(..)
  , liftParseError
  ) where

import Text.Parsec.Error

import Kitten.Location
import Kitten.Util.Either

data CompileError
  = CompileError Location String
  | InternalError String
  | TypeError Location String

instance Show CompileError where
  show compileError = case compileError of
    CompileError location message -> concat
      [ show location
      , ": compile error: "
      , message
      ]
    InternalError message -> "internal error: " ++ message
    TypeError location message -> concat
      [ show location
      , ": type error: "
      , message
      ]

liftParseError :: Either ParseError a -> Either CompileError a
liftParseError = mapLeft $ \ parseError -> let
    location = Location
      { locationStart = errorPos parseError
      , locationIndent = 0  -- FIXME
      }
    message = showErrorMessages
      "or" "unknown" "expecting" "unexpected" "end of input"
      $ errorMessages parseError
  in
    CompileError location message
