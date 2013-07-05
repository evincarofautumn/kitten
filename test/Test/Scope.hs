module Test.Scope where

import Control.Monad
import Test.Hspec

import Test.Util

import Kitten.ClosedName
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Scope
import Kitten.Typed

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ do
    testScope
      (Fragment [] [] [function [scoped [push $ local 0]]])
      (Fragment [] [] [closure [] [scoped [push $ local 0]]])

  describe "non-nested closure" $ do
    testScope
      (Fragment [] [] [scoped [function [push $ local 0]]])
      (Fragment [] [] [scoped [closure [closedName 0] [push $ closed 0]]])

  describe "nested closure" $ do
    testScope
      (Fragment [] [] [scoped [function [scoped [function [push $ local 1, push $ local 0, biAdd]]]]])
      (Fragment [] []
        [scoped
          [ closure [closedName 0]
            [ scoped
              [ closure [reclosedName 0, closedName 0]
                [ push $ closed 0
                , push $ closed 1
                , biAdd]]]]])

testScope
  :: Fragment Value Typed -> Fragment Value Typed -> Spec
testScope source expected = let
    actual = scope source
  in
    it (show source) . unless (actual == expected)
      $ expectedButGot (show expected) (show actual)

biAdd :: Typed
biAdd = Builtin Builtin.AddInt TestLocation

closed :: Int -> Value
closed index = Closed (Name index)

closedName :: Int -> ClosedName
closedName = ClosedName . Name

closure :: [ClosedName] -> [Typed] -> Typed
closure names terms = push $ Closure names (compose terms)

compose :: [Typed] -> Typed
compose = Compose

function :: [Typed] -> Typed
function terms = push $ Function (compose terms)

local :: Int -> Value
local index = Local (Name index)

push :: Value -> Typed
push value = Push value TestLocation

reclosedName :: Int -> ClosedName
reclosedName = ReclosedName . Name

scoped :: [Typed] -> Typed
scoped terms = Scoped (compose terms) TestLocation

word :: Int -> Typed
word index = Call (Name index) TestLocation
