{-# LANGUAGE OverloadedStrings #-}

module Test.Scope where

import Control.Monad
import Data.Monoid
import Test.Hspec

import qualified Data.Vector as V

import Kitten.Location
import Kitten.Scope
import Kitten.Types
import Test.Util

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
biAdd = TrIntrinsic InAddInt TestLocation

closed :: Int -> ResolvedValue
closed index = TrClosed index TestLocation

closure :: [ClosedName] -> [ResolvedTerm] -> ResolvedTerm
closure names terms = push $ TrClosure
  (V.fromList names) (compose terms) TestLocation

compose :: [ResolvedTerm] -> ResolvedTerm
compose terms = TrCompose StackAny (V.fromList terms) TestLocation

function :: [ResolvedTerm] -> ResolvedTerm
function terms = push $ TrQuotation (compose terms) TestLocation

local :: Int -> ResolvedValue
local index = TrLocal index TestLocation

push :: ResolvedValue -> ResolvedTerm
push value = TrPush value TestLocation

scoped :: [ResolvedTerm] -> ResolvedTerm
scoped terms = TrLambda "testvar" (compose terms) TestLocation
