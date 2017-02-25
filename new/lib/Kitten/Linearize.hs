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
import Kitten.Name (Closed(..), ClosureIndex(..), LocalIndex(..))
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Origin (Origin)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import qualified Kitten.Operator as Operator
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary

import Debug.Trace (trace)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

-- | Linearization (I didn't know what else to call it) instruments copies and
-- drops of local variables with invocations of @_::copy@ (copy constructor) and
-- @_::destroy@ (destructor), respectively.
--
-- A variable is copied if it appears twice or more in its scope; it's dropped
-- if it doesn't appear at all, or if an explicit drop is present due to an
-- ignored local (@_@). The first appearance of a local is moved, not copied, so
-- this doesn't instrument anything if the variable is used only once.

linearize :: Term () -> Term ()
linearize = (\x -> trace (Pretty.render $ pPrint x) x) . snd . go []
  where

  go :: [Int] -> Term () -> ([Int], Term ())
  go counts0 term = case term of
    Coercion{} -> (counts0, term)
    Compose _ a b -> let
      (counts1, a') = go counts0 a
      (counts2, b') = go counts1 b
      in (counts2, Compose () a' b')
    Generic x body origin -> let
      (counts1, body') = go counts0 body
      in (counts1, Generic x body' origin)
    Group{} -> error "group should not appear after desugaring"
    Lambda _ x varType body origin -> let
      (n : counts1, body') = go (0 : counts0) body
      body'' = case n of
        0 -> instrumentDrop origin body'
        1 -> body'
        _ -> instrumentCopy body'
      in (counts1, Lambda () x varType body'' origin)
    -- FIXME: count usages for each branch & take maximum?
    Match hint _ cases else_ origin -> let

      (counts1, mElse') = goElse counts0 else_
      (counts2, cases') = first (map maximum . transpose)
        $ unzip $ map (goCase counts0) cases
      in (zipWith max counts1 counts2, Match hint () cases' mElse' origin)
      where

      goCase :: [Int] -> Case () -> ([Int], Case ())
      goCase counts (Case name body caseOrigin) = let
        (counts1, body') = go counts body
        in (counts1, Case name body' caseOrigin)

      goElse :: [Int] -> Else () -> ([Int], Else ())
      goElse counts (Else body elseOrigin) = let
        (counts1, body') = go counts body
        in (counts1, Else body' elseOrigin)

    New{} -> (counts0, term)
    NewClosure{} -> (counts0, term)
    NewVector{} -> (counts0, term)
    Push _ (Local (LocalIndex index)) _ -> let
      (h, t : ts) = splitAt index counts0
      in (h ++ succ t : ts, term)
    Push _ (Capture closed body) origin -> let
      go (ClosedLocal (LocalIndex index)) acc = let
        (h, t : ts) = splitAt index acc
        in h ++ succ t : ts
      go (ClosedClosure (ClosureIndex index)) acc
        = acc  -- TODO: count closure variables (maybe by rewriting them into locals)
      in (foldr go counts0 closed, term)
{-
    Push _ Quotation{} _ -> error
      "pushing of quotation should not appear after desugaring"
-}
    Push{} -> (counts0, term)
    Word{} -> (counts0, term)

instrumentDrop :: Origin -> Term () -> Term ()
instrumentDrop origin a = Term.compose () origin
  [ a
  , Push () (Local (LocalIndex 0)) origin
  , Word () Operator.Postfix
    (QualifiedName (Qualified Vocabulary.global "destroy")) [] origin
  ]

instrumentCopy :: Term () -> Term ()
instrumentCopy = go 0
  where

  go :: Int -> Term () -> Term ()
  go n term = case term of
    Coercion{} -> term
    Compose _ a b -> Compose () (go n a) (go n b)
    Generic x body origin -> Generic x (go n body) origin
    Group{} -> error "group should not appear after desugaring"
    Lambda _ name varType body origin
      -> Lambda () name varType (go (succ n) body) origin
    Match hint _ cases else_ origin
      -> Match hint () (map goCase cases) (goElse else_) origin
      where

      goCase :: Case () -> Case ()
      goCase (Case name body caseOrigin) = Case name (go n body) caseOrigin

      goElse :: Else () -> Else ()
      goElse (Else body elseOrigin) = Else (go n body) elseOrigin

    New{} -> term
    NewClosure{} -> term
    NewVector{} -> term
    Push _ (Local (LocalIndex index)) origin
      | index == n
      -> Compose () term $ Word () Operator.Postfix
        (QualifiedName (Qualified Vocabulary.global "copy")) [] origin
    Push{} -> term
    Word{} -> term
