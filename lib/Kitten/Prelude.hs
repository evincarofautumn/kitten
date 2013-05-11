{-# LANGUAGE RecordWildCards #-}

module Kitten.Prelude
  ( compilePrelude
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Kitten.Def
import Kitten.Compile
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Resolved

compilePrelude :: IO (Either CompileError [Def Resolved])
compilePrelude
  = liftM (either (fail . show) return) $ runEitherT compiled
  where
  compiled :: EitherT CompileError IO [Def Resolved]
  compiled = do
    preludeSource <- lift $ readFile preludeFile
    Fragment{..} <- hoistEither $ compile [] [] preludeFile preludeSource
    unless (null $ fragmentTerms)
      . left $ CompileError UnknownLocation
      "prelude includes executable code"
    return fragmentDefs

preludeFile :: FilePath
preludeFile = "prelude.ktn"
