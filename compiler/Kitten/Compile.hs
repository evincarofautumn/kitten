module Kitten.Compile
  ( compile
  , typecheck
  ) where

import Control.Monad
import Data.Vector (Vector)

import Kitten.Anno
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve
import Kitten.Term
import Kitten.Token
import Kitten.Type
import Kitten.Type.Infer

import qualified Kitten.Resolve as Resolve

compile
  :: [Resolve.Value]
  -> Vector (Def Resolved)
  -> Vector (Anno Name)
  -> String
  -> String
  -> Either CompileError (Fragment Resolved Name)
compile stack prelude annos name source = do
  tokenized <- failIfError $ tokenize name source
  parsed <- failIfError $ parse name tokenized
  resolved <- resolveFragment prelude parsed
  void $ typeFragment prelude annos stack resolved
  return resolved

typecheck
  :: [Resolve.Value]
  -> Vector (Def Resolved)
  -> Vector (Anno Name)
  -> String
  -> String
  -> Either CompileError Type
typecheck stack prelude annos name
  = failIfError . tokenize name
  >=> failIfError . parse name
  >=> resolveFragment prelude
  >=> liftM (manifestType . fragmentTerm)
    . typeFragment prelude annos stack
