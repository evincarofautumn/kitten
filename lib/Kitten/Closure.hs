{-|
Module      : Kitten.Closure
Description : Converting closures to explicitly capture locals
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Kitten.Closure
  ( convertClosures
  ) where

import Data.Functor.Foldable
import Data.HashSet (HashSet)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Kitten.Name (Unqualified(..))
import Kitten.Origin (Origin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..), SweetF(..))
import Kitten.Text (capitalize)
import qualified Data.HashSet as HashSet
import qualified Kitten.Term as Term

convertClosures :: Sweet 'Postfix -> Sweet 'Scoped
convertClosures = (Term.scopedFromPostfix .) $ cata $ \ case
{-
  LambdaF _ name _ body origin -> let
    vars = filter (/= LocalIndex 0) $ freeVars body
    in Term.compose () origin
      $ map (\ index -> Word () Operator.Postfix (LocalName index) [] origin) vars ++
      [ close vars origin $ lambdas origin body (LocalIndex 0 : vars)
      ]
-}
  -- FIXME: Should anything be done with capture?
  SFQuotation _ origin body -> let
    vars = sort $ HashSet.toList $ freeVars body
    in Term.composed origin
      $ map (SLocal () origin) vars ++
      [close True vars origin body]
  -- TODO: unboxed quotations
  term -> embed term
  where
    close :: Bool -> [Unqualified] -> Origin -> Sweet 'Postfix -> Sweet 'Postfix
    close boxed vars@(_ : _) origin body = SCompose ()
      (SQuotation () origin
        (SLambda () origin
          (map (\ var -> (origin, Just var, ())) vars)
          body))
      -- We haven't done type inference yet, so we don't know the type of the
      -- closure, but all we need at this point is the number of fields;
      -- inference will fill in their types.
      --
      -- x y z { ... } pack (_, _, _) as [X, Y, Z] (_)
      (SPack () origin boxed
        (map (\ (Unqualified var)
          -> ((), Unqualified $ capitalize var)) vars)
        ())

    -- FIXME: For consistency, this might generate an existential that
    -- quantifies over no types.
    close _boxed [] _origin body = body

freeVars :: Sweet p -> HashSet Unqualified
freeVars = cata $ \ case
  SFArray _ _ items -> mconcat items
  SFAs{} -> mempty
  SFCharacter{} -> mempty
  SFCompose _ a b -> a <> b
  SFDo _ _ f x -> f <> x
  SFEscape _ _ body -> body
  SFFloat{} -> mempty
  SFGeneric _ _ _ _body -> error
    "generic expressions should not appear before type inference"
  SFGroup _ _ body -> body
  SFIdentity{} -> mempty
  SFIf _ _ mCondition true elifs mElse -> mconcat
    [ fromMaybe mempty mCondition
    , true
    , foldMap (\ (_, condition, body) -> condition <> body) elifs
    , fromMaybe mempty mElse
    ]
  SFInfix _ _ left _ right _ -> left <> right
  SFInteger{} -> mempty
  SFJump{} -> mempty
  SFLambda _ _ vars body -> HashSet.difference body
    $ foldMap (\ (_, mName, _) -> maybe mempty HashSet.singleton mName) vars
  SFList _ _ items -> mconcat items
  SFLocal _ _ name -> HashSet.singleton name
  SFLoop{} -> mempty
  SFMatch _ _ mScrutinee cases mElse -> mconcat
    [ fromMaybe mempty mScrutinee
    , foldMap (\ (_, _, body) -> body) cases
    , fromMaybe mempty mElse
    ]
  SFNestableCharacter{} -> mempty
  SFNestableText{} -> mempty
  SFPack{} -> mempty
  SFParagraph{} -> mempty
  SFTag{} -> mempty
  SFText{} -> mempty
  SFQuotation _ _ body -> body
  SFReturn{} -> mempty
  SFSection _ _ _ _ operand _ -> operand
  SFTodo{} -> mempty
  SFUnboxedQuotation _ _ body -> body
  SFWith{} -> mempty
  -- I'm pretty sure these should never be locals at this point.
  SFWord _ _ _ _ _ -> mempty
