{-|
Module      : Kitten.Flatten
Description : Lifting quotations into top-level definitions
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Flatten
  ( flatten
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, gets, modify, runStateT)
import Data.Foldable (foldlM)
import Data.Monoid ((<>))
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (inferType0)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Name (Qualified(..), Unqualified(..))
import Kitten.Name (GeneralName(..), Qualifier)
import Kitten.Origin (getOrigin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..))
import Kitten.Type (Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Free as Free
import qualified Kitten.Operator as Operator
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.TypeEnv as TypeEnv

newtype LambdaIndex = LambdaIndex Int

-- | Lifts quotations in a 'Term' into top-level definitions, within the
-- vocabulary referenced by a 'Qualifier', adding them to the 'Dictionary'.

flatten
  :: Dictionary
  -> Qualifier
  -> Sweet 'Typed
  -> K (Sweet 'Typed, Dictionary)

flatten dictionary qualifier term0 = do
  ((term', _), (_, dictionary')) <- flip runStateT (LambdaIndex 0, dictionary)
    $ go TypeEnv.empty term0
  return (term', dictionary')
  where

  go
    :: TypeEnv
    -> Sweet 'Typed
    -> StateT (LambdaIndex, Dictionary) K (Sweet 'Typed, TypeEnv)
  go tenv0 term = case term of

    SArray type_ origin items -> do
      (reversedItems', tenv1) <- foldlM (\ (acc, tenv) item -> do
        (item', tenv') <- go tenv item
        pure (item' : acc, tenv')) ([], tenv0) items
      pure (SArray type_ origin $ reverse reversedItems', tenv1)

    SAs type_ origin types -> pure (SAs type_ origin types, tenv0)

    SCharacter type_ origin text -> pure (SCharacter type_ origin text, tenv0)

    SCompose type_ a b -> do
      (a', tenv1) <- go tenv0 a
      (b', tenv2) <- go tenv1 b
      pure (SCompose type_ a' b', tenv2)

    SDo type_ origin f x -> do
      (f', tenv1) <- go tenv0 f
      (x', tenv2) <- go tenv1 x
      pure (SDo type_ origin f' x', tenv2)

    -- An escaped name is left as-is, because we're converting quotations to
    -- escaped names.
    SEscape type_ origin (SWord wordType wordOrigin fixity name typeArgs)
      -> pure
        ( SEscape type_ origin $ SWord wordType wordOrigin fixity name typeArgs
        , tenv0
        )

    -- Any other escaped term is treated as a quotation.
    SEscape type_ origin body -> go tenv0 $ SQuotation type_ origin body

    SFloat type_ origin literal -> pure (SFloat type_ origin literal, tenv0)

    SGeneric origin name x body -> do
      (body', tenv1) <- go tenv0 body
      pure (SGeneric origin name x body', tenv1)

    SGroup type_ origin body -> do
      (body', tenv1) <- go tenv0 body
      pure (SGroup type_ origin body', tenv1)

    SIdentity type_ origin -> pure (SIdentity type_ origin, tenv0)

    SIf type_ origin mCondition true elifs mElse -> do
      (mCondition', tenv1) <- case mCondition of
        Just condition -> do
          (condition', tenv') <- go tenv0 condition
          pure (Just condition', tenv')
        Nothing -> pure (Nothing, tenv0)
      (true', tenv2) <- go tenv1 true
      (reversedElifs', tenv3) <- foldlM
        (\ (acc, tenv) (elifOrigin, condition, body) -> do
          (condition', tenv') <- go tenv condition
          (body', tenv'') <- go tenv' body
          pure ((elifOrigin, condition', body') : acc, tenv''))
        ([], tenv2)
        elifs
      (mElse', tenv4) <- case mElse of
        Just else_ -> do
          (else', tenv') <- go tenv3 else_
          pure (Just else', tenv')
        Nothing -> pure (Nothing, tenv3)
      pure
        ( SIf type_ origin mCondition' true' (reverse reversedElifs') mElse'
        , tenv4
        )

    SInfix type_ origin left operator right typeArgs -> do
      (left', tenv1) <- go tenv0 left
      (right', tenv2) <- go tenv1 right
      pure (SInfix type_ origin left' operator right' typeArgs, tenv2)

    SInteger type_ origin literal -> pure
      (SInteger type_ origin literal, tenv0)

    SJump type_ origin -> pure (SJump type_ origin, tenv0)

    SLambda type_ origin vars body -> do
      let
        oldLocals = TypeEnv.vs tenv0
        varTypes = foldMap (\ (_, mName, varType)
          -> maybe mempty (\ name -> HashMap.singleton name varType) mName) vars
        localEnv = tenv0 { TypeEnv.vs = TypeEnv.vs tenv0 <> varTypes }
      (body', tenv1) <- go localEnv body
      let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
      return (SLambda type_ origin vars body', tenv2)

    SList type_ origin items -> do
      (reversedItems', tenv1) <- foldlM (\ (acc, tenv) item -> do
        (item', tenv') <- go tenv item
        pure (item' : acc, tenv')) ([], tenv0) items
      let items' = reverse reversedItems'
      pure (SList type_ origin items', tenv1)

    SLocal type_ origin name -> pure (SLocal type_ origin name, tenv0)

    SLoop type_ origin -> pure (SLoop type_ origin, tenv0)

    SMatch type_ origin mScrutinee cases mElse -> do
      (mScrutinee', tenv1) <- case mScrutinee of
        Just scrutinee -> do
          (scrutinee', tenv') <- go tenv0 scrutinee
          pure (Just scrutinee', tenv')
        Nothing -> pure (Nothing, tenv0)
      (reversedCases', tenv2) <- foldlM (\ (acc, tenv) (caseOrigin, name, body) -> do
        (body', tenv') <- go tenv body
        pure ((caseOrigin, name, body') : acc, tenv')) ([], tenv1) cases
      let cases' = reversedCases'
      (mElse', tenv3) <- case mElse of
        Just else_ -> do
          (else', tenv') <- go tenv2 else_
          pure (Just else', tenv')
        Nothing -> pure (Nothing, tenv2)
      pure (SMatch type_ origin mScrutinee' cases' mElse', tenv3)

    SNestableCharacter type_ origin text
      -> pure (SNestableCharacter type_ origin text, tenv0)

    SNestableText type_ origin text
      -> pure (SNestableText type_ origin text, tenv0)

    SPack type_ origin boxed vars hiddenType
      -> pure (SPack type_ origin boxed vars hiddenType, tenv0)

    SParagraph type_ origin text
      -> pure (SParagraph type_ origin text, tenv0)

    STag type_ origin size index
      -> pure (STag type_ origin size index, tenv0)

    SText type_ origin text
      -> pure (SText type_ origin text, tenv0)

    SQuotation _type origin body -> do
      (body', tenv1) <- go tenv0 body
      LambdaIndex index <- gets fst
      let
        name = Qualified qualifier
          $ Unqualified $ Text.pack $ "lambda." ++ show index
      modify $ \ (_, d) -> (LambdaIndex $ succ index, d)
      let
        deducedType = Term.annotation body
        type_ = foldr addForall deducedType
          $ Map.toList $ Free.tvks tenv1 deducedType
        addForall (i, (n, k)) = Forall origin (Var n i k)
      modify $ \ (l, d) -> let
        entry = Entry.Word
          Category.Word
          Merge.Deny
          (getOrigin body')
          Nothing
          (Just (Signature.Type type_))
          (Just body')
        in (l, Dictionary.insert (Instantiated name []) entry d)
      dict <- gets snd
      let
        tenv2 = tenv1
          { TypeEnv.sigs = Map.insert name type_ $ TypeEnv.sigs tenv1 }
      -- FIXME: Should this not discard the type environment?
      (typechecked, _) <- lift $ inferType0 dict tenv2 Nothing
        $ SEscape () origin
        $ SWord () (getOrigin body) Operator.Postfix (QualifiedName name) []
      return (typechecked, tenv2)

    SReturn type_ origin -> pure (SReturn type_ origin, tenv0)

    SSection type_ origin name swap operand typeArgs -> do
      (operand', tenv1) <- go tenv0 operand
      pure (SSection type_ origin name swap operand' typeArgs, tenv1)

    STodo type_ origin -> pure (STodo type_ origin, tenv0)

    -- At this point, boxed and unboxed quotations are differentiated by the
    -- closure packing expressions that use them, so we can reuse the boxed case
    -- for the unboxed case.
    SUnboxedQuotation type_ origin body
      -> go tenv0 (SQuotation type_ origin body)

    SWith type_ origin permits -> pure (SWith type_ origin permits, tenv0)

    SWord type_ origin fixity name typeArgs
      -> pure (SWord type_ origin fixity name typeArgs, tenv0)
