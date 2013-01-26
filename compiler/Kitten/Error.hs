module Kitten.Error
  ( CompileError(..)
  , failIfError
  ) where

import Data.Text (Text)
import Text.Parsec.Error

import qualified Data.Text as Text

import Kitten.Util

data CompileError = CompileError !Text

instance Show CompileError where
  show (CompileError message) = Text.unpack message

failIfError :: Either ParseError a -> Either CompileError a
failIfError = mapLeft $ CompileError . Text.pack . show
