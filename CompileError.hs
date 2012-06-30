module CompileError (CompileError(..)) where

import Data.List (intercalate)
import Text.Parsec as Parsec
import Text.Parsec.Error as Parsec

data CompileError
  = CompileError String
  | ParseError Parsec.ParseError

instance Show CompileError where
  show (CompileError message) = message
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
