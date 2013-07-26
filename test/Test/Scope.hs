module Test.Scope where

import Control.Monad
import Data.Monoid
import Test.Hspec

import Test.Util

import Kitten.ClosedName
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolved
import Kitten.Scope

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ do
    testScope
      mempty { fragmentTerms = [function [scoped [push $ local 0]]] }
      mempty { fragmentTerms = [closure [] [scoped [push $ local 0]]] }

  describe "non-nested closure" $ do
    testScope
      mempty { fragmentTerms = [scoped [function [push $ local 0]]] }
      mempty { fragmentTerms = [scoped [closure [closedName 0] [push $ closed 0]]] }

  describe "nested closure" $ do
    testScope
      mempty { fragmentTerms = [scoped [function [scoped [function [push $ local 1, push $ local 0, biAdd]]]]] }
      mempty { fragmentTerms =
        [scoped
          [ closure [closedName 0]
            [ scoped
              [ closure [reclosedName 0, closedName 0]
                [ push $ closed 0
                , push $ closed 1
                , biAdd]]]]] }

testScope
  :: Fragment Value Resolved -> Fragment Value Resolved -> Spec
testScope source expected = let
    actual = scope source
  in
    it (show source) . unless (actual == expected)
      $ expectedButGot (show expected) (show actual)

biAdd :: Resolved
biAdd = Builtin Builtin.AddInt TestLocation

closed :: Int -> Value
closed index = Closed (Name index)

closedName :: Int -> ClosedName
closedName = ClosedName . Name

closure :: [ClosedName] -> [Resolved] -> Resolved
closure names terms = push $ Closure names (compose terms)

compose :: [Resolved] -> Resolved
compose terms = Compose terms TestLocation

function :: [Resolved] -> Resolved
function terms = push $ Function (compose terms)

local :: Int -> Value
local index = Local (Name index)

push :: Value -> Resolved
push value = Push value TestLocation

reclosedName :: Int -> ClosedName
reclosedName = ReclosedName . Name

scoped :: [Resolved] -> Resolved
scoped terms = Scoped (compose terms) TestLocation

word :: Int -> Resolved
word index = Call (Name index) TestLocation
