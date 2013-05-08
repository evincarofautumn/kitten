{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck
  ( typecheck
  ) where

import Control.Monad
import Control.Monad.Trans.State

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Resolve (Resolved(..), Value(..))
import Kitten.Typecheck.Term
import Kitten.Typecheck.Monad

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either CompileError ()
typecheck _prelude stack Fragment{..}
  = flip evalStateT emptyEnv
  { envDefs = fragmentDefs
  } $ do
    mapM_ typecheckValue stack
    mapM_ typecheckDef fragmentDefs
    typecheckTerms fragmentTerms

typecheckDef :: Def Resolved -> Typecheck
typecheckDef Def{..} = withLocation defLocation
  . void . hypothetically $ typecheckTerm defTerm
