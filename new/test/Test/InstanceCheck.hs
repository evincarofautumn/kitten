{-# LANGUAGE OverloadedStrings #-}

module Test.InstanceCheck
  ( spec
  ) where


import Kitten.Informer (checkpoint)
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Kind (Kind(..))
import Kitten.Monad (runKitten)
import Kitten.Name (Qualified(..))
import Kitten.Type (Type(..), TypeId(..), Var(..))
import Test.Common
import Test.HUnit (assertBool)
import Test.Hspec (Spec, it)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Origin as Origin
import qualified Kitten.Type as Type
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

spec :: Spec
spec = do
  it "with concrete types" $ do

    --    Int32
    -- <: Int32
    testInstanceCheck Positive int int

    --    <T> (T)
    -- <: Int32
    testInstanceCheck Positive (fx x) int

    --    <R..., T, +P> (R... -> R..., T +P)
    -- <: <R..., +P>    (R... -> R..., Int32 +P)
    testInstanceCheck Positive
      (fr $ fe $ fx $ Type.fun o r (Type.prod o r x) e)
      (fr $ fe $ Type.fun o r (Type.prod o r int) e)

  it "with parameterized types" $ do

    --    <A, B> (Pair<A, B>)
    -- <: <A>    (Pair<A, A>)
    testInstanceCheck Positive
      (fx $ fy $ pair :@ x :@ y)
      (fx $ pair :@ x :@ x)

    --     <A, B> (Pair<A, B> -> Pair<B, A>)
    -- </: <A, B> (Pair<A, B> -> Pair<A, B>)
    testInstanceCheck Negative
      (fr $ fx $ fy $ fe $ Type.fun o
        (Type.prod o r (pair :@ x :@ y))
        (Type.prod o r (pair :@ y :@ x)) e)
      (fr $ fx $ fy $ fe $ Type.fun o
        (Type.prod o r (pair :@ x :@ y))
        (Type.prod o r (pair :@ x :@ y)) e)

  where
  o = Origin.point "" 0 0
  r = TypeVar o rv
  s = TypeVar o sv
  x = TypeVar o xv
  y = TypeVar o yv
  e = TypeVar o ev
  rv = Var (TypeId 0) Stack
  sv = Var (TypeId 1) Stack
  xv = Var (TypeId 2) Value
  yv = Var (TypeId 3) Value
  ev = Var (TypeId 4) Permission
  fr = Type.Forall o rv
  fs = Type.Forall o sv
  fx = Type.Forall o xv
  fy = Type.Forall o yv
  fe = Type.Forall o ev
  ctor = TypeConstructor o . Type.Constructor
    . Qualified Vocabulary.global
  char = ctor "Char"
  int = ctor "Int32"
  io = ctor "IO"
  float = ctor "Float64"
  pair = ctor "Pair"

testInstanceCheck :: Sign -> Type -> Type -> IO ()
testInstanceCheck sign a b = do
  result <- runKitten $ do
    instanceCheck "polymorphic" a "concrete" b
    checkpoint
  case sign of
    Positive -> assertBool (Pretty.render $ Pretty.hsep [pPrint a, "<:", pPrint b])
      $ either (const False) (const True) result
    Negative -> assertBool (Pretty.render $ Pretty.hsep [pPrint a, "</:", pPrint b])
      $ either (const True) (const False) result
