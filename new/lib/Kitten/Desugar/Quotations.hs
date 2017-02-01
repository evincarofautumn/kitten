{-|
Module      : Kitten.Desugar.Quotations
Description : Lifting anonymous functions
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Desugar.Quotations
  ( desugar
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, gets, modify, runStateT)
import Data.Foldable (foldrM)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (inferType0, valueKinded)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Name (Closed(..), Qualified(..), Qualifier, Unqualified(..))
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Free as Free
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.TypeEnv as TypeEnv

import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty
import Control.Monad.IO.Class
import qualified Kitten.Quantify as Quantify

newtype LambdaIndex = LambdaIndex Int

-- | Lifts quotations in a 'Term' into top-level definitions, within the
-- vocabulary referenced by a 'Qualifier', adding them to the 'Dictionary'.

desugar
  :: Dictionary
  -> Qualifier
  -> Term Type
  -> K (Term Type, Dictionary)
desugar dictionary qualifier term0 = do
  ((term', _), (_, dictionary')) <- flip runStateT (LambdaIndex 0, dictionary)
    $ go TypeEnv.empty term0
  return (term', dictionary')
  where

  go
    :: TypeEnv
    -> Term Type
    -> StateT (LambdaIndex, Dictionary) K (Term Type, TypeEnv)
  go tenv0 term = case term of
    Coercion{} -> done
    Compose type_ a b -> do
      (a', tenv1) <- go tenv0 a
      (b', tenv2) <- go tenv1 b
      return (Compose type_ a' b', tenv2)
    Generic type_ a origin -> do
      (a', tenv1) <- go tenv0 a
      return (Generic type_ a' origin, tenv1)
    Lambda type_ name varType a origin -> do
      let
        oldLocals = TypeEnv.vs tenv0
        localEnv = tenv0 { TypeEnv.vs = varType : TypeEnv.vs tenv0 }
      (a', tenv1) <- go localEnv a
      let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
      return (Lambda type_ name varType a' origin, tenv2)
    Match hint type_ cases else_ origin -> do
      (cases', tenv1) <- foldrM
        (\ (Case name a caseOrigin) (acc, tenv) -> do
          (a', tenv') <- go tenv a
          return (Case name a' caseOrigin : acc, tenv')) ([], tenv0) cases
      (else', tenv2) <- case else_ of
        Else a elseOrigin -> do
          (a', tenv') <- go tenv1 a
          return (Else a' elseOrigin, tenv')
      return (Match hint type_ cases' else' origin, tenv2)
    New{} -> done
    NewClosure{} -> done
    NewVector{} -> done
    Push _type (Capture closed a) origin -> do
      let
        types = map (TypeEnv.getClosed tenv0) closed
        oldClosure = TypeEnv.closure tenv0
        localEnv = tenv0 { TypeEnv.closure = types }
      (a', tenv1) <- go localEnv a
      let tenv2 = tenv1 { TypeEnv.closure = oldClosure }
      LambdaIndex index <- gets fst
      let
        name = Qualified qualifier
          $ Unqualified $ Text.pack $ "lambda" ++ show index
      modify $ \ (_, d) -> (LambdaIndex $ succ index, d)
      let
        deducedType = Term.type_ a
        freeVars = Map.toList $ Free.tvks tenv2 deducedType
        type_ = foldr (uncurry ((Forall origin .) . Var)) deducedType
          freeVars
        a'' = Quantify.term type_ a'
      liftIO $ putStrLn $ Pretty.render $ Pretty.hcat
        ["Lifted quotation ", pPrint name, " with type ", pPrint type_, " and body ", pPrint a'']
      modify $ \ (l, d) -> let
        entry = Entry.Word
          Category.Word
          Merge.Deny
          (Term.origin a'')
          Nothing
          (Just (Signature.Type type_))
          (Just a'')
        in (l, Dictionary.insert (Instantiated name []) entry d)
      dict <- gets snd
      (typechecked, _) <- lift $ inferType0 dict tenv2 Nothing
        $ Term.compose () origin $ map pushClosed closed ++
          [ Push () (Name (Instantiated name [])) origin
          , NewClosure () (length closed) origin
          ]
      return (typechecked, tenv2)
      where

      pushClosed :: Closed -> Term ()
      pushClosed name = Push () (case name of
        ClosedLocal index -> Local index
        ClosedClosure index -> Closed index) origin

    Push{} -> done
    Word{} -> done
    where
    done = return (term, tenv0)
