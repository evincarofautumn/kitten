module Kitten.Compile
  ( compile
  ) where

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Resolve
import Kitten.Scope
import Kitten.Term
import Kitten.Token
import Kitten.Typecheck

import qualified Kitten.Resolve as Resolve

compile
  :: [Resolve.Value]
  -> [Def Resolved]
  -> String
  -> String
  -> Either CompileError (Fragment Resolved)
compile stack prelude name source = do
  tokenized <- liftParseError $ tokenize name source
  parsed <- liftParseError $ parse name tokenized
  resolved <- resolve prelude parsed
  typecheck prelude stack resolved
  return $ scope resolved
