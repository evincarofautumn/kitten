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
import Data.Foldable (foldlM, foldrM)
import Kitten.Dictionary (Dictionary)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (inferType0)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Monad (K)
import Kitten.Name (Closed(..), Qualified(..), Qualifier, Unqualified(..))
import Kitten.Name (Qualifier)
import Kitten.Origin (getOrigin)
import Kitten.Phase (Phase(..))
import Kitten.Phase (Phase(..))
import Kitten.Term (Case(..), Else(..), Sweet(..), Term(..), Value(..))
import Kitten.Term (Sweet)
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

    SLambda type_ origin vars body -> error "TODO: flatten lambda"
{-
      let
        oldLocals = TypeEnv.vs tenv0
        localEnv = tenv0 { TypeEnv.vs = varTypes ++ TypeEnv.vs tenv0 }
      (a', tenv1) <- go localEnv a
      let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
      return (Lambda type_ name varType a' origin, tenv2)
-}

    _ -> error "TODO: finish flatten"

{-
    SList           -- Boxed list literal
    (Annotation p)
    !Origin
    [Sweet p]       -- List elements

    SLocal          -- Push local
    (Annotation p)
    !Origin
    !Unqualified    -- Local name

    SLoop
    (Annotation p)
    !Origin

    SMatch
    (Annotation p)
    !Origin
    !(Maybe (Sweet p))                -- Scrutinee
    [(Origin, GeneralName, Sweet p)]  -- case branches
    !(Maybe (Sweet p))                -- else branch

    SNestableCharacter  -- Nestable character literal (round quotes)
    (Annotation p)
    !Origin
    !Text               -- Escaped character (e.g., " " vs. "")

    SNestableText   -- Nestable text literal (round quotes)
    (Annotation p)
    !Origin
    !Text           -- Escaped text

    SParagraph      -- Paragraph literal
    (Annotation p)
    !Origin
    !Text           -- Escaped text ("\n" for line breaks, "\\n" for escapes)

    STag               -- Tag fields to make new ADT instance
    (Annotation p)
    !Origin
    !Int               -- Number of fields
    !ConstructorIndex  -- Constructor tag

    SText           -- Text literal (straight quotes)
    (Annotation p)
    !Origin
    !Text           -- Escaped text

    SQuotation      -- Boxed quotation
    (Annotation p)
    !Origin
    !(Sweet p)

    SReturn
    (Annotation p)
    !Origin

    SSection                       -- Operator section
    (Annotation p)
    !Origin
    !GeneralName                   -- Operator
    !(Either (Sweet p) (Sweet p))  -- Operand

    STodo           -- ... expression
    (Annotation p)
    !Origin

    SUnboxedQuotation
    (Annotation p)
    !Origin
    !(Sweet p)

    SWith           -- Permission coercion
    (Annotation p)
    !Origin
    [Permit]        -- Permissions to add or remove

    SWord
    (Annotation p)
    !Origin
    !Fixity         -- Whether used infix or postfix at call site
    !GeneralName
    [Signature]     -- Type arguments

-}
