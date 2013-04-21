module Test.Scope where

import Control.Monad
import Test.Hspec

import Test.Util

import Kitten.Anno (Anno)
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve
import Kitten.Scope

import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ do
    testScope
      (fragment [] [] [fun [Scoped [local 0]]])
      (fragment [] [] [closure [] [Scoped [local 0]]])

  describe "non-nested closure" $ do
    testScope
      (fragment [] [] [Scoped [fun [local 0]]])
      (fragment [] [] [Scoped [closure [0] [closed 0, Scoped [closed 0]]]])

  describe "nested closure" $ do
    testScope
      (fragment [] [] [Scoped [fun [Scoped [fun [local 1, local 0, biAdd]]]]])
      (fragment [] []
        [ Scoped
          [ closure [0]
            [ closed 0
            , Scoped
              [ Scoped
                [ closure [1, 0]
                  [ closed 0
                  , Scoped
                    [ closed 1
                    , Scoped
                      [ closed 0
                      , closed 1
                      , biAdd]]]]]]]])

testScope :: Fragment Resolved -> Fragment Resolved -> Spec
testScope source expected = let
    actual = scope source
  in
    it (show source) . unless (actual == expected)
      $ expectedButGot (show expected) (show actual)

biAdd :: Resolved
biAdd = Builtin Builtin.Add

biFun :: Resolved
biFun = Builtin Builtin.Fun

biCompose :: Resolved
biCompose = Builtin Builtin.Compose

closed :: Int -> Resolved
closed = Closed . Name

closure :: [Int] -> [Resolved] -> Resolved
closure names = Push . Closure (map Name names)

fragment
  :: [Anno]
  -> [Def Resolved]
  -> [Resolved]
  -> Fragment Resolved
fragment annos defs terms
  = Fragment annos defs terms

fun :: [Resolved] -> Resolved
fun = Push . Fun

local :: Int -> Resolved
local = Local . Name

word :: Int -> Resolved
word = Push . Word . Name
