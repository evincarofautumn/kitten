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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Coerce (coerce)
import Data.Functor.Foldable
import Data.HashSet (HashSet)
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Debug.Trace
import Kitten.Name (Closed(..), ClosureIndex(..), GeneralName(..), LocalIndex(..), Unqualified(..))
import Kitten.Origin (Origin)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet(..), SweetF(..))
import Kitten.Term (Sweet)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Kitten.Operator as Operator
import qualified Kitten.Term as Term
import qualified Text.PrettyPrint as Pretty

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
      [ close vars origin
        $ SLambda () origin
          (map (\ var -> (origin, Just var, ())) vars) body
      ]
  term -> embed term
  where
    close :: [Unqualified] -> Origin -> Sweet 'Postfix -> Sweet 'Postfix
    close [] _origin body = body
    close vars origin body = SCompose ()
      (SQuotation () origin body)
      (SIdentity () origin) -- FIXME: should be NewClosure () origin (length vars)

freeVars :: Sweet p -> HashSet Unqualified
freeVars = cata $ \ case
  SFArray _ _ items -> mconcat items
  SFAs{} -> mempty
  SFCharacter{} -> mempty
  SFCompose _ a b -> a <> b
  SFDo _ _ f x -> f <> x
  SFEscape _ _ body -> body
  SFFloat{} -> mempty
  SFGeneric _ _ _ body -> error
    "generic expressions should not appear before type inference"
  SFGroup _ _ body -> body
  SFIdentity{} -> mempty
  SFIf _ _ mCondition true elifs mElse -> mconcat
    [ fromMaybe mempty mCondition
    , true
    , foldMap (\ (_, condition, body) -> condition <> body) elifs
    , fromMaybe mempty mElse
    ]
  SFInfix _ _ left _ right -> left <> right
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
  SFParagraph{} -> mempty
  SFTag{} -> mempty
  SFText{} -> mempty
  SFQuotation _ _ body -> body
  SFReturn{} -> mempty
  SFSection _ _ _ _ operand -> operand
  SFTodo{} -> mempty
  SFUnboxedQuotation _ _ body -> body
  SFWith{} -> mempty
  -- I'm pretty sure these should never be locals at this point.
  SFWord _ _ _ _ _ -> mempty
