{-# LANGUAGE OverloadedStrings #-}

module Test.Scope where

import Control.Monad
import Data.Monoid
import Test.Hspec

import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.Fragment
import Kitten.Location
import Kitten.Scope
import Kitten.Tree
import Kitten.Type (StackHint(..))
import Test.Util

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ testScope
    mempty { fragmentTerms = V.fromList [function [scoped [push $ local 0]]] }
    mempty { fragmentTerms = V.fromList [closure [] [scoped [push $ local 0]]] }

  describe "non-nested closure" $ testScope
    mempty { fragmentTerms = V.fromList [scoped [function [push $ local 0]]] }
    mempty { fragmentTerms = V.fromList [scoped [closure [ClosedName 0] [push $ closed 0]]] }

  describe "nested closure" $ testScope
    mempty { fragmentTerms = V.fromList [scoped [function [scoped [function [push $ local 1, push $ local 0, biAdd]]]]] }
    mempty { fragmentTerms = V.fromList
      [scoped
        [ closure [ClosedName 0]
          [ scoped
            [ closure [ReclosedName 0, ClosedName 0]
              [ push $ closed 0
              , push $ closed 1
              , biAdd]]]]] }

testScope
  :: Fragment ResolvedTerm -> Fragment ResolvedTerm -> Spec
testScope source expected
  = let actual = scope source
  in it (show source) . unless (actual == expected)
    $ expectedButGot (show expected) (show actual)

biAdd :: ResolvedTerm
biAdd = Builtin Builtin.AddInt TestLocation

closed :: Int -> ResolvedValue
closed index = Closed index TestLocation

closure :: [ClosedName] -> [ResolvedTerm] -> ResolvedTerm
closure names terms = push $ Closure
  (V.fromList names) (compose terms) TestLocation

compose :: [ResolvedTerm] -> ResolvedTerm
compose terms = Compose StackAny (V.fromList terms) TestLocation

function :: [ResolvedTerm] -> ResolvedTerm
function terms = push $ Quotation (compose terms) TestLocation

local :: Int -> ResolvedValue
local index = Local index TestLocation

push :: ResolvedValue -> ResolvedTerm
push value = Push value TestLocation

scoped :: [ResolvedTerm] -> ResolvedTerm
scoped terms = Lambda "testvar" (compose terms) TestLocation
