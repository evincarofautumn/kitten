module Error
  ( CompileError(..)
  ) where

data CompileError
  = CompileError String

instance Show CompileError where
  show (CompileError message) = message
