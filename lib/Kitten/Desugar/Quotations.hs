{-|
Module      : Kitten.Desugar.Quotations
Description : Lifting anonymous functions
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Desugar.Quotations
  ( desugar
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, gets, modify, runStateT)
import Data.Foldable (foldrM)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (inferType0)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (K)
import Kitten.Name (Closed(..), Qualified(..), Qualifier, Unqualified(..))
import Kitten.Origin (getOrigin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Case(..), Else(..), Sweet(..), Term(..), Value(..))
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

desugar
  :: Dictionary
  -> Qualifier
  -> Sweet 'Typed
  -> K (Sweet 'Typed, Dictionary)
desugar dictionary qualifier term0 = do
  ((term', _), (_, dictionary')) <- flip runStateT (LambdaIndex 0, dictionary)
    $ go TypeEnv.empty term0
  return (term', dictionary')
  where

  go
    :: TypeEnv
    -> Sweet 'Typed
    -> StateT (LambdaIndex, Dictionary) K (Sweet 'Typed, TypeEnv)
  go tenv0 term = case term of

    SArray type_ origin items
      -> SArray type_ origin <$> mapM go items

    SAs{} -> done
    SCharacter{} -> done

    SCompose type_ a b -> do
      (a', tenv1) <- go tenv0 a
      (b', tenv2) <- go tenv1 b
      pure (SCompose type_ a' b', tenv2)

    SDo type_ origin f x -> do
      (f', tenv1) <- go tenv0 f
      (x', tenv2) <- go tenv1 x
      pure (SDo type_ origin f' x', tenv2)

    SEscape type_ origin x -> error "TODO: lift escape"

{-*******************************************************************************
I got to around here when I realised that 'Scope' and 'Quotations' need to be
merged into a single closure-conversion/lambda-lifting pass, so it doesn't make
sense to keep going here.
*******************************************************************************-}

{-
    SFloat
    (Annotation p)
    !Origin
    !FloatLiteral

    {- (HasTypes p ~ 'True) => -} SGeneric  -- Local generic type variables
    (Annotation p)
    !Origin
    !Unqualified                            -- Type name
    !TypeId                                 -- Type ID (could be removed?)
    !(Sweet p)                              -- Body

    SGroup
    (Annotation p)
    !Origin
    !(Sweet p)

    SIdentity
    (Annotation p)
    !Origin

    SIf
    (Annotation p)
    !Origin
    !(Maybe (Sweet p))    -- Condition
    !(Sweet p)            -- True branch
    [(Origin, Sweet p, Sweet p)]  -- elif branches
    !(Maybe (Sweet p))    -- else branch

    (HasInfix p ~ 'True) => SInfix  -- Desugared infix operator (only present after infix desugaring)
    (Annotation p)
    !Origin
    !(Sweet p)                      -- Left operand
    !GeneralName                    -- Operator
    !(Sweet p)                      -- Right operand

    SInteger
    (Annotation p)
    !Origin
    !IntegerLiteral

    SJump
    (Annotation p)
    !Origin

    SLambda                                      -- Local variable
    (Annotation p)
    !Origin
    [(Origin, Maybe Unqualified, Annotation p)]  -- Names or ignores (TODO: allow signatures)
    !(Sweet p)                                   -- Body

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
    !Text               -- Escaped character (e.g., " " vs. ""

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

{-

    Coercion{} -> done
    Compose type_ a b -> do
      (a', tenv1) <- go tenv0 a
      (b', tenv2) <- go tenv1 b
      return (Compose type_ a' b', tenv2)
    Generic name type_ a origin -> do
      (a', tenv1) <- go tenv0 a
      return (Generic name type_ a' origin, tenv1)
    Group{} -> error "group should not appear after infix desugaring"
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
        type_ = foldr addForall deducedType
          $ Map.toList $ Free.tvks tenv2 deducedType
        addForall (i, (n, k)) = Forall origin (Var n i k)
      modify $ \ (l, d) -> let
        entry = Entry.Word
          Category.Word
          Merge.Deny
          (getOrigin a')
          Nothing
          (Just (Signature.Type type_))
          (Just a')
        in (l, Dictionary.insert (Instantiated name []) entry d)
      dict <- gets snd
      (typechecked, _) <- lift $ inferType0 dict tenv2 Nothing
        $ Term.compose () origin $ map pushClosed closed ++
          [ Push () (Name name) origin
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

-}

    where
      done = return (term, tenv0)
