module Kitten.Compile
  ( compile
  , typecheck
  ) where

import Control.Monad

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Resolve
import Kitten.Term
import Kitten.Token
import Kitten.Type

compile
  :: [Def Resolved]
  -> String
  -> String
  -> Either CompileError (Fragment Resolved)
compile prelude name source = do
  tokenized <- failIfError $ tokenize name source
  parsed <- failIfError $ parse name tokenized
  resolved <- resolveFragment prelude parsed
  void $ typeFragment resolved
  return resolved

typecheck
  :: [Def Resolved]
  -> String
  -> String
  -> Either CompileError Type
typecheck prelude name
  = failIfError . tokenize name
  >=> failIfError . parse name
  >=> resolveFragment prelude
  >=> liftM (manifestType . fragmentTerm) . typeFragment
