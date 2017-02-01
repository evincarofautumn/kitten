{-|
Module      : Kitten.Linearize
Description : Instrumentation of copies and drops
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}

module Kitten.Linearize
  ( linearize
  ) where

import Control.Arrow (first)
import Data.List (transpose)
import Kitten.Name (GeneralName(..), LocalIndex(..), Qualified(..))
import Kitten.Origin (Origin)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type)
import qualified Kitten.Operator as Operator
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary

-- | Linearization replaces all copies and drops with explicit invocations of
-- the @_::copy@ and @_::drop@ words. A value is copied if it appears twice or
-- more in its scope; it's dropped if it doesn't appear at all, or if an
-- explicit @drop@ is present due to an ignored local (@_@). If it only appears
-- once, it is moved, and no special word is invoked.
--
-- FIXME: This is experimental and subject to change.

linearize :: Term Type -> Term Type
linearize = snd . go []
  where

  go :: [Int] -> Term Type -> ([Int], Term Type)
  go counts0 term = case term of
    Coercion{} -> (counts0, term)
    Compose type_ a b -> let
      (counts1, a') = go counts0 a
      (counts2, b') = go counts1 b
      in (counts2, Compose type_ a' b')
    Generic x body origin -> let
      (counts1, body') = go counts0 body
      in (counts1, Generic x body' origin)
    Lambda type_ x varType body origin -> let
      (n : counts1, body') = go (0 : counts0) body
      body'' = case n of
        0 -> instrumentDrop origin varType body'
        1 -> body'
        _ -> instrumentCopy varType body'
      in (counts1, Lambda type_ x varType body'' origin)
    -- FIXME: count usages for each branch & take maximum
    Match hint type_ cases else_ origin -> let

      (counts1, mElse') = goElse counts0 else_
      (counts2, cases') = first (map maximum . transpose)
        $ unzip $ map (goCase counts0) cases
      in (zipWith max counts1 counts2, Match hint type_ cases' mElse' origin)
      where

      goCase :: [Int] -> Case Type -> ([Int], Case Type)
      goCase counts (Case name body caseOrigin) = let
        (counts1, body') = go counts body
        in (counts1, Case name body' caseOrigin)

      goElse :: [Int] -> Else Type -> ([Int], Else Type)
      goElse counts (Else body elseOrigin) = let
        (counts1, body') = go counts body
        in (counts1, Else body' elseOrigin)

    New{} -> (counts0, term)
    NewClosure{} -> (counts0, term)
    NewVector{} -> (counts0, term)
    Push _ (Local (LocalIndex index)) _ -> let
      (h, t : ts) = splitAt index counts0
      in (h ++ succ t : ts, term)
    Push _ Capture{} _ -> error
      "pushing of capture should not appear after desugaring"
    Push _ Quotation{} _ -> error
      "pushing of quotation should not appear after desugaring"
    Push{} -> (counts0, term)
    Word{} -> (counts0, term)

instrumentDrop :: Origin -> Type -> Term Type -> Term Type
instrumentDrop origin type_ a = Term.compose todoTyped origin
  [ a
  , Push todoTyped (Local (LocalIndex 0)) origin
  , Word todoTyped Operator.Postfix
    (QualifiedName (Qualified Vocabulary.global "drop")) [type_] origin
  ]

instrumentCopy :: Type -> Term Type -> Term Type
instrumentCopy varType = go 0
  where

  go :: Int -> Term Type -> Term Type
  go n term = case term of
    Coercion{} -> term
    Compose type_ a b -> Compose type_ (go n a) (go n b)
    Generic x body origin -> Generic x (go n body) origin
    Lambda type_ name varType' body origin
      -> Lambda type_ name varType' (go (succ n) body) origin
    Match hint type_ cases else_ origin
      -> Match hint type_ (map goCase cases) (goElse else_) origin
      where

      goCase :: Case Type -> Case Type
      goCase (Case name body caseOrigin) = Case name (go n body) caseOrigin

      goElse :: Else Type -> Else Type
      goElse (Else body elseOrigin) = Else (go n body) elseOrigin

    New{} -> term
    NewClosure{} -> term
    NewVector{} -> term
    Push _ (Local (LocalIndex index)) origin
      | index == n
      -> Compose todoTyped term $ Word todoTyped Operator.Postfix
        (QualifiedName (Qualified Vocabulary.global "copy")) [varType] origin
    Push{} -> term
    Word{} -> term

todoTyped :: a
todoTyped = error "TODO: generate typed terms"
