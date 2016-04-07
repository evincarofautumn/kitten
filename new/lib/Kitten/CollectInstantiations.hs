{-# LANGUAGE OverloadedStrings #-}

module Kitten.CollectInstantiations
  ( collectInstantiations
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldrM)
import Data.List (unfoldr)
import Kitten.Dictionary (Dictionary)
import Kitten.Informer (Informer(..))
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), Qualified(..), Unqualified(..))
import Kitten.Queue (Queue)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Constructor(..), Type(..))
import Kitten.TypeEnv (TypeEnv)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Parent as Parent
import qualified Kitten.Infer as Infer
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Kind as Kind
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Queue as Queue
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

-- In order to support unboxed generics, for every call site of a generic
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

collectInstantiations :: TypeEnv -> Dictionary -> K Dictionary
collectInstantiations tenv0 dictionary0 = do

-- We enqueue all the instantiation sites reachable from the top level of the
-- program, and any non-generic definitions.

  (entries, q0) <- foldrM
    (\ original@(name, entry) (acc, q) -> case entry of
      Entry.Word category merge origin parent signature (Just term) -> do
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
-- enqueueing them for instantiation. We perform the actual instantiation while
-- processing the queue, so as to avoid redundant instantiations.

  go :: InstantiationQueue -> Term Type -> K (Term Type, InstantiationQueue)
  go q0 term = case term of
    Call{} -> proceed
    Compose type_ a b -> do
      (a', q1) <- go q0 a
      (b', q2) <- go q1 b
      return (Compose type_ a' b', q2)
    Drop{} -> proceed

-- If the definition is generic, we simply ignore it; we won't find any
-- instantiations in it, because it's not instantiated itself!

    Generic{} -> proceed
    Group{} -> error "group should not appear after linearization"
    Identity{} -> proceed
    If type_ true false origin -> do
      (true', q1) <- go q0 true
      (false', q2) <- go q1 false
      return (If type_ true' false' origin, q2)
    Lambda type_ name varType body origin -> do
      (body', q1) <- go q0 body
      return (Lambda type_ name varType body' origin, q1)
    Match type_ cases mElse origin -> do
      (cases', q1) <- foldrM (\ (Case name body caseOrigin) (bodies, q) -> do
        (body', q') <- go q body
        return (Case name body' caseOrigin : bodies, q'))
        ([], q0) cases
      (mElse', q2) <- case mElse of
        Just (Else body elseOrigin) -> do
          (body', q') <- go q1 body
          return (Just (Else body' elseOrigin), q')
        Nothing -> return (Nothing, q1)
      return (Match type_ cases' mElse' origin, q2)
    New{} -> proceed
    NewClosure{} -> proceed
    NewVector _ _size elementType _origin -> do
      q1 <- instantiateTypes dictionary0 elementType q0
      return (term, q1)
    Push _ Quotation{} _ -> error
      "quotation should not appear after quotation desugaring"
    Push{} -> proceed
    Swap{} -> proceed
    With{} -> proceed
    Word type_ fixity (QualifiedName name) args origin -> do
      let
        types = case Dictionary.lookup (Instantiated name []) dictionary0 of
          -- If this is a constructor call, generate an instantiation of the
          -- type it's constructing.
          Just (Entry.Word _ _ _ (Just (Parent.Type typeName)) _ _)
            -> Just (typeName, args)
          _ -> Nothing
        q1 = foldr Queue.enqueue q0 types
      return
        ( Word type_ fixity
          (UnqualifiedName $ Unqualified $ Mangle.name $ Instantiated name args)
          [] origin
        , Queue.enqueue (name, args) q1
        )
    -- FIXME: Should calls to non-qualified names even be around at this point?
    Word{} -> proceed
    where
    proceed = return (term, q0)

-- Processing the queue operates by dequeueing and instantiating each definition
-- in turn. If the definition has already been instantiated, we simply proceed.

  processQueue :: InstantiationQueue -> Dictionary -> K Dictionary
  processQueue q dictionary = case Queue.dequeue q of
    Nothing -> return dictionary
    Just ((name, args), q')
      -> case Dictionary.lookup (Instantiated name args) dictionary of
        Just{} -> processQueue q' dictionary
        Nothing -> case Dictionary.lookup (Instantiated name []) dictionary of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' dictionary
          Just (Entry.Word category merge origin parent signature mTerm)
            -> case mTerm of
              Just term -> do
                term' <- while origin
                  (Pretty.hsep ["instantiating", Pretty.quote name])
                  $ Instantiate.term tenv0 term args
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
            let
              entry' = Entry.Type origin params ctors
            -- Maybe generate instantiations for constructors?
            processQueue q'' $ Dictionary.insert
              (Instantiated name args)
              entry'
              dictionary
          Just entry -> error $ Pretty.render $ Pretty.hcat
            ["attempt to instantiate non-word ", Pretty.quote name, ":", pPrint entry]

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
          return $ Queue.enqueue instantiation q0
        -- TODO: Forall.
        _ -> return q0
      _ -> return q0
  collect args t = case t of
    a :@ b -> do
      -- go b
      collect (b : args) a
    TypeConstructor _ (Constructor name) -> do
      -- go a
      return (name, args)
    _ -> error "non-constructor in type application (requires HKTs)"
