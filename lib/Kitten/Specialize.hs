{-|
Module      : Kitten.Specialize
Description : Generic instantiation collection
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Specialize
  ( specialize
  ) where

import Data.Foldable (foldrM)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (typeSize)
import Kitten.Informer (Informer(..))
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Phase (Phase(..))
import Kitten.Queue (Queue)
import Kitten.Term (Sweet(..))
import Kitten.Type (Constructor(..), Type(..))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Infer as Infer
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Kind as Kind
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Queue as Queue
import qualified Kitten.TypeEnv as TypeEnv
import qualified Text.PrettyPrint as Pretty

-- | In order to support unboxed generics, for every call site of a generic
-- definition in a program, we produce a specialized instantiation of the
-- definition with the value-kinded type parameters set to the given type
-- arguments. This is transitive: if a generic definition calls another generic
-- definition with one of its own generic type parameters as a type argument,
-- then an instantiation must also be generated of the called definition.
--
-- For generic types, we also generate specializations; this is mainly to have
-- the size and alignment requirements on hand during code generation. A generic
-- type is instantiated if it's mentioned in the signature of any instantiated
-- word definition.
--
-- This takes the names of definitions to use as the starting point from which
-- to specialize, so that we don't need to traverse the the whole dictionary
-- when collecting specializations for a small amount of newly added code, as in
-- interactive mode.

specialize :: Maybe [Instantiated] -> Dictionary -> K Dictionary
specialize mStart dictionary0 = do

-- If a set of starting points is specified, we process the bodies of those
-- definitions; otherwise, we process all non-generic definitions.

  (entries, q0) <- foldrM
    (\ original@(name, entry) (acc, q) -> case entry of
      Entry.Word category merge origin parent signature (Just term)
        | fromMaybe True $ (name `elem`) <$> mStart -> do
        (term', q') <- go q term
        let
          entry' = Entry.Word
            category merge origin parent signature $ Just term'
        return ((name, entry') : acc, q')
      _ -> return (original : acc, q))
    ([], Queue.empty)
    $ Dictionary.toList dictionary0

-- Next, we process the queue. Doing so may enqueue new instantiation sites for
-- processing; however, this is guaranteed to halt because the number of actual
-- instantiations is finite.

  processQueue q0 $ Dictionary.fromList entries
  where

  -- We process a definition in a single pass, mangling all its call sites and
  -- enqueueing them for instantiation. We perform the actual instantiation
  -- while processing the queue, so as to avoid redundant instantiations.
  --
  -- FIXME: Does this need to visit all annotated types to collect type
  -- instantiations, or just type arguments?

  go
    :: InstantiationQueue
    -> Sweet 'Typed
    -> K (Sweet 'Typed, InstantiationQueue)
  go q0 term = case term of

    SArray type_ origin items -> do
      (items', q1) <- foldrM (\ item (acc, q) -> do
        (item', q') <- go q item
        pure (item' : acc, q')) ([], q0) items
      pure (SArray type_ origin items', q1)

    -- FIXME: Should instantiate types.
    SAs{} -> proceed

    SCharacter{} -> proceed

    SCompose type_ a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      pure (SCompose type_ a' b', q2)

    SDo type_ origin f x -> do
      (f', q1) <- go q0 f
      (x', q2) <- go q1 x
      pure (SDo type_ origin f' x', q2)

    SEscape type_ origin body -> do
      (body', q1) <- go q0 body
      pure (SEscape type_ origin body', q1)

    SFloat{} -> proceed

    -- If the definition is generic, we simply ignore it; we won't find any
    -- instantiations in it, because it's not instantiated itself!
    SGeneric{} -> proceed

    SGroup type_ origin body -> do
      (body', q1) <- go q0 body
      pure (SGroup type_ origin body', q1)

    SIdentity{} -> proceed

    SIf type_ origin mCondition true elifs mElse -> do
      (mCondition', q1) <- case mCondition of
        Just condition -> do
          (condition', q') <- go q0 condition
          pure (Just condition', q')
        Nothing -> pure (Nothing, q0)
      (true', q2) <- go q1 true
      (elifs', q3) <- foldrM (\ (elifOrigin, condition, body) (acc, q) -> do
        (condition', q') <- go q condition
        (body', q'') <- go q' body
        pure ((elifOrigin, condition', body') : acc, q''))
        ([], q2) elifs
      (mElse', q4) <- case mElse of
        Just else_ -> do
          (else', q') <- go q3 else_
          pure (Just else', q')
        Nothing -> pure (Nothing, q3)
      pure (SIf type_ origin mCondition' true' elifs' mElse', q4)

    SInfix type_ origin left op right typeArgs -> do
      (left', q1) <- go q0 left
      (right', q2) <- go q1 right
      q3 <- foldrM (instantiateTypes dictionary0) q2 typeArgs
      pure (SInfix type_ origin left' op right' typeArgs, q3)

    SInteger{} -> proceed

    SJump{} -> proceed

    SLambda type_ origin vars body -> do
      (body', q1) <- go q0 body
      pure (SLambda type_ origin vars body', q1)

    SList type_ origin items -> do
      (items', q1) <- foldrM (\ item (acc, q) -> do
        (item', q') <- go q item
        pure (item' : acc, q')) ([], q0) items
      pure (SList type_ origin items', q1)

    SLocal{} -> proceed

    SLoop{} -> proceed

    SMatch type_ origin mScrutinee cases mElse -> do
      (mScrutinee', q1) <- case mScrutinee of
        Just scrutinee -> do
          (scrutinee', q') <- go q0 scrutinee
          pure (Just scrutinee', q')
        Nothing -> pure (Nothing, q0)
      (cases', q2) <- foldrM (\ (caseOrigin, name, body) (acc, q) -> do
        (body', q') <- go q body
        pure ((caseOrigin, name, body') : acc, q')) ([], q1) cases
      (mElse', q3) <- case mElse of
        Just else_ -> do
          (else', q') <- go q2 else_
          pure (Just else', q')
        Nothing -> pure (Nothing, q2)
      pure (SMatch type_ origin mScrutinee' cases' mElse', q3)

    SNestableCharacter{} -> proceed

    SNestableText{} -> proceed

    SPack{} -> proceed

    SParagraph{} -> proceed

    -- FIXME: If this is a constructor call, this should generate an
    -- instantiation of the type it's constructing.
    STag{} -> proceed

    SText{} -> proceed

    SQuotation type_ origin body -> do
      (body', q1) <- go q0 body
      pure (SQuotation type_ origin body', q1)

    SReturn{} -> proceed

    SSection type_ origin name swap operand typeArgs -> do
      (operand', q1) <- go q0 operand
      q2 <- foldrM (instantiateTypes dictionary0) q1 typeArgs
      pure (SSection type_ origin name swap operand' typeArgs, q2)

    STodo{} -> proceed

    SUnboxedQuotation type_ origin body -> do
      (body', q1) <- go q0 body
      pure (SUnboxedQuotation type_ origin body', q1)

    SWith{} -> proceed

    SWord _ _ _ (QualifiedName name) typeArgs -> do
      q1 <- foldrM (instantiateTypes dictionary0) q0 typeArgs
      -- FIXME: Should this return a mangled name?
      pure (term, Queue.enqueue (name, typeArgs) q1)

    SWord _ origin _ name typeArgs -> error $ Pretty.render $ Pretty.hcat
      [ pPrint origin
      , ": cannot specialize word with unresolved name "
      , Pretty.quote name
      , "::<"
      , Pretty.list $ map pPrint typeArgs
      , ">"
      ]

    where
    proceed = pure (term, q0)

-- Processing the queue operates by dequeueing and instantiating each definition
-- in turn. If the definition has already been instantiated, we simply proceed.

  processQueue :: InstantiationQueue -> Dictionary -> K Dictionary
  processQueue q dictionary = case Queue.dequeue q of
    Nothing -> pure dictionary
    Just ((name, args), q')
      -> case Dictionary.lookup (Instantiated name args) dictionary of
        -- The name is already instantiated: move along.
        Just{} -> processQueue q' dictionary
        Nothing -> case Dictionary.lookup (Instantiated name []) dictionary of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' dictionary
          Just (Entry.Word category merge origin parent signature mTerm)
            -> case mTerm of
              Just term -> do
                term' <- while origin
                  (Pretty.hsep ["instantiating", Pretty.quote name])
                  $ Instantiate.term TypeEnv.empty term args
                (term'', q'') <- go q' term'
                let
                  entry' = Entry.Word category merge origin parent signature
                    $ Just term''
                processQueue q'' $ Dictionary.insert
                  (Instantiated name args)
                  entry'
                  dictionary
              -- There should be no need to instantiate declarations, as they
              -- should only refer to intrinsics.
              Nothing -> processQueue q' dictionary
          Just (Entry.Type origin params ctors) -> do
            q'' <- foldrM (instantiateTypes dictionary0) q' args
            type_ <- Infer.dataType origin params ctors dictionary0
            TypeValue _ size <- typeSize dictionary0 $ foldl' (:@) type_ args
            let entry' = Entry.InstantiatedType origin size
            -- Maybe generate instantiations for constructors?
            processQueue q'' $ Dictionary.insert
              (Instantiated name args)
              entry'
              dictionary
          Just entry -> error $ Pretty.render $ Pretty.hcat
            [ "attempt to instantiate non-word "
            , Pretty.quote name
            , "::<"
            , Pretty.list $ map pPrint args
            , ">: "
            , pPrint entry
            ]

type InstantiationQueue = Queue Instantiation

type Instantiation = (Qualified, [Type])

instantiateTypes
  :: Dictionary -> Type -> InstantiationQueue -> K InstantiationQueue
instantiateTypes dictionary type0 q0 = go type0
  where
  go type_ = do
    kind <- Infer.typeKind dictionary type_
    case kind of
      Kind.Value -> case type_ of
        (:@){} -> do
          instantiation <- collect [] type_
          pure $ Queue.enqueue instantiation q0
        -- TODO: Forall.
        _ -> pure q0
      _ -> pure q0
  collect args t = case t of
    a :@ b -> do
      -- go b
      collect (b : args) a
    TypeConstructor _ (Constructor name) -> do
      -- go a
      pure (name, args)
    _ -> error "non-constructor in type application (requires HKTs)"
