{-# LANGUAGE OverloadedStrings #-}

module Test.Scope where

import Control.Monad
import Data.Monoid
import Test.Hspec

import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Scope
import Kitten.Tree
import Test.Util

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ testScope
    mempty { fragmentTerms = V.fromList [function [scoped [push $ local 0]]] }
    mempty { fragmentTerms = V.fromList [closure [] [scoped [push $ local 0]]] }

  describe "non-nested closure" $ testScope
    mempty { fragmentTerms = V.fromList [scoped [function [push $ local 0]]] }
    mempty { fragmentTerms = V.fromList [scoped [closure [closedName 0] [push $ closed 0]]] }

  describe "nested closure" $ testScope
    mempty { fragmentTerms = V.fromList [scoped [function [scoped [function [push $ local 1, push $ local 0, biAdd]]]]] }
    mempty { fragmentTerms = V.fromList
      [scoped
        [ closure [closedName 0]
          [ scoped
            [ closure [reclosedName 0, closedName 0]
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
closed index = Closed (Name index) TestLocation

closedName :: Int -> ClosedName
closedName = ClosedName . Name

closure :: [ClosedName] -> [ResolvedTerm] -> ResolvedTerm
closure names terms = push $ Closure
  (V.fromList names) (compose terms) TestLocation

compose :: [ResolvedTerm] -> ResolvedTerm
compose terms = Compose (V.fromList terms) TestLocation

function :: [ResolvedTerm] -> ResolvedTerm
function terms = push $ Function (compose terms) TestLocation

local :: Int -> ResolvedValue
local index = Local (Name index) TestLocation

push :: ResolvedValue -> ResolvedTerm
push value = Push value TestLocation

reclosedName :: Int -> ClosedName
reclosedName = ReclosedName . Name

scoped :: [ResolvedTerm] -> ResolvedTerm
scoped terms = Lambda "testvar" (compose terms) TestLocation

word :: Int -> ResolvedTerm
word index = Call (Name index) TestLocation
