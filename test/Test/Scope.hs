module Test.Scope where

import Control.Monad
import Test.Hspec

import Test.Util

import Kitten.Anno (Anno(..))
import Kitten.Def
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolved
import Kitten.Scope

import qualified Kitten.Anno as Anno
import qualified Kitten.Builtin as Builtin

spec :: Spec
spec = do
  describe "no change" $ do
    testScope
      (fragment [] [function [scoped [local 0]]])
      (fragment [] [closure [] [scoped [local 0]]])

  describe "non-nested closure" $ do
    testScope
      (fragment [] [scoped [function [local 0]]])
      (fragment [] [scoped [closure [0] [closed 0, scoped [closed 0]]]])

  describe "nested closure" $ do
    testScope
      (fragment [] [scoped [function [scoped [function [local 1, local 0, biAdd]]]]])
      (fragment []
        [ scoped
          [ closure [0]
            [ closed 0
            , scoped
              [ scoped
                [ closure [1, 0]
                  [ closed 0
                  , scoped
                    [ closed 1
                    , scoped
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
biAdd = Builtin Builtin.AddInt TestLocation

biFunction :: Resolved
biFunction = Builtin Builtin.Function TestLocation

biCompose :: Resolved
biCompose = Builtin Builtin.Compose TestLocation

closed :: Int -> Resolved
closed index = Closed (Name index) TestLocation

closure :: [Int] -> [Resolved] -> Resolved
closure names terms
  = Push (Closure (map Name names) terms) TestLocation

fragment
  :: [Def Resolved]
  -> [Resolved]
  -> Fragment Resolved
fragment defs terms
  = Fragment defs terms

function :: [Resolved] -> Resolved
function terms
  = Push (Function emptyAnno terms) TestLocation
  where emptyAnno = Anno Anno.Any TestLocation

local :: Int -> Resolved
local index = Local (Name index) TestLocation

word :: Int -> Resolved
word index = Push (Word (Name index)) TestLocation

scoped :: [Resolved] -> Resolved
scoped terms = Scoped terms TestLocation
