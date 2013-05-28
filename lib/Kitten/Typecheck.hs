{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck
  ( typecheck
  ) where

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Resolved
import Kitten.Typecheck.Term
import Kitten.Typecheck.Monad

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either [CompileError] (Fragment Resolved)
typecheck prelude stack fragment@Fragment{..}
  = evalTypecheck emptyEnv
  { envDefs = prelude ++ fragmentDefs
  } $ do
    mapM_ typecheckValue stack
    (defs, terms) <- guardLiftM2 (,)
      (guardMapM typecheckDef fragmentDefs)
      (typecheckTerms fragmentTerms)
    return fragment
      { fragmentDefs = defs
      , fragmentTerms = terms
      }

typecheckDef :: Def Resolved -> Typecheck (Def Resolved)
typecheckDef def@Def{..} = withLocation defLocation $ do
  term <- typecheckTerm defTerm
  return def { defTerm = term }
