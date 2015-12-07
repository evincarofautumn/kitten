{-# LANGUAGE OverloadedStrings #-}

module Kitten.CollectInstantiations
  ( collectInstantiations
  ) where

import Data.Foldable (foldrM)
import Data.HashMap.Strict (HashMap)
import Data.List (find)
import Kitten.Informer (Informer(..))
import Kitten.Monad (K)
import Kitten.Name (GeneralName(..), Qualified(..), Unqualified(..))
import Kitten.Program (Program)
import Kitten.Queue (Queue)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type)
import Kitten.TypeEnv (TypeEnv)
import Kitten.Vocabulary (globalVocabulary)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Program as Program
import qualified Kitten.Queue as Queue
import qualified Kitten.Term as Term

-- In order to support unboxed generics, for every call site of a generic
-- definition in a program, we produce a specialized instantiation of the
-- definition with the value-kinded type parameters set to the given type
-- arguments. This is transitive: if a generic definition calls another generic
-- definition with one of its own generic type parameters as a type argument,
-- then an instantiation must also be generated of the called definition.

collectInstantiations :: TypeEnv -> Program -> K Program
collectInstantiations tenv0 program0 = do

-- We enqueue all the instantiation sites reachable from the top level of the
-- program, and any non-generic definitions.

  (definitions, q0) <- foldrM
    (\ ((name, type_), expr) (acc, q) -> do
      (expr', q') <- go q expr
      return (((name, type_), expr') : acc, q'))
    ([], Queue.empty)
    $ HashMap.toList $ Program.definitions program0

-- Next, we process the queue. Doing so may enqueue new instantiation sites for
-- processing; however, this is guaranteed to halt because the number of actual
-- instantiations is finite.

  definitions' <- processQueue q0 $ HashMap.fromList definitions
  return program0 { Program.definitions = definitions' }

  where

-- We process a definition in a single pass, mangling all its call sites and
-- enqueueing them for instantiation. We perform the actual instantiation while
-- processing the queue, so as to avoid redundant instantiations.

  go :: InstantiationQueue -> Term -> K (Term, InstantiationQueue)
  go q0 term = case term of
    Call type_ fixity (QualifiedName name) args origin -> return
      ( Call type_ fixity
        (UnqualifiedName (Unqualified (Mangle.name name args))) [] origin
      , Queue.enqueue (name, args) q0
      )
    -- FIXME: Should calls to non-qualified names even be around at this point?
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
    Intrinsic{} -> proceed
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
    NewVector{} -> proceed
    Push _ Quotation{} _ -> error
      "quotation should not appear after quotation desugaring"
    Push{} -> proceed
    Swap{} -> proceed
    where
    proceed = return (term, q0)

-- Processing the queue operates by dequeueing and instantiating each definition
-- in turn. If the definition has already been instantiated, we simply proceed.

  processQueue
    :: InstantiationQueue
    -> HashMap (Qualified, Type) Term
    -> K (HashMap (Qualified, Type) Term)
  processQueue q defs = case Queue.dequeue q of
    Nothing -> return defs
    Just ((name, args), q') -> let
      mangled = Mangle.name name args
      name' = Qualified globalVocabulary $ Unqualified mangled
      in case lookupWith ((name' ==) . fst) defs of
        Just{} -> processQueue q' defs
        Nothing -> case lookupWith ((name ==) . fst) defs of
          -- The name is not user-defined, so it doesn't need to be mangled.
          Nothing -> processQueue q' defs
          Just ((_, type_), term) -> do
            term' <- while ["instantiating", Pretty.quote name] (Term.origin term)
              $ Instantiate.term tenv0 term args
            (term'', q'') <- go q' term'
            processQueue q'' $ HashMap.insert
              (Qualified globalVocabulary (Unqualified mangled), type_)
              term'' defs

type InstantiationQueue = Queue (Qualified, [Type])

-- FIXME: This should be made more efficient.
lookupWith :: (k -> Bool) -> HashMap k v -> Maybe (k, v)
lookupWith f = find (f . fst) . HashMap.toList
