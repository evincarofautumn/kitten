{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Infer.Unify
  ( shouldBe
  , unifyM
  , unifyM_
  , unifyVar
  ) where

import Control.Monad
import Data.Function

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Infer.Locations
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Program
import Kitten.Type
import Kitten.Type.Tidy
import Kitten.Util.FailWriter
import Kitten.Util.Text (ToText(..))

-- | Simplifies and unifies two types.
unify
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Program
  -> Either [ErrorGroup] Program
unify have want program
  = (unification `on` simplify program) have want program

class Unification a where
  unification
    :: Type a
    -> Type a
    -> Program
    -> Either [ErrorGroup] Program

instance Unification Stack where
  unification have want program = case (have, want) of
    _ | have == want -> Right program
    (TyStack a b, TyStack c d) -> unify b d program >>= unify a c
    (TyVar var _, type_)
      -> unifyVar var type_ program
    (type_, TyVar var _)
      -> unifyVar var type_ program
    _ -> Left $ unificationError Finite loc have want
    where
    loc = inferenceLocation program

instance Unification Scalar where
  unification have want program = case (have, want) of
    _ | have == want -> Right program
    (TyProduct a b _, TyProduct c d _) -> unify b d program >>= unify a c
    (TyOption a _, TyOption b _) -> unify a b program
    (TyApply a as _, b) | V.null as -> unify a b program
    (a, TyApply b bs _) | V.null bs -> unify a b program
    (TyApply a as _, TyApply b bs _) -> do
      program' <- unify a b program
      if V.length as /= V.length bs
        then Left $ unificationError Finite loc have want
        else V.foldM (\p (x, y) -> unify x y p) program' $ V.zip as bs
    (TySum a b _, TySum c d _) -> unify b d program >>= unify a c
    (TyFunction a b _, TyFunction c d _)
      -> unify b d program >>= unify a c
    (TyVector a _, TyVector b _) -> unify a b program
    (TyVar var _, type_)
      -> unifyVar var type_ program
    (type_, TyVar var _)
      -> unifyVar var type_ program
    (TyQuantified scheme _, type_) -> let
      (type', program') = instantiate loc scheme program
      in unify type' type_ program'
    (type_, TyQuantified scheme _) -> let
      (type', program') = instantiate loc scheme program
      in unify type' type_ program'
    _ -> Left $ unificationError Finite loc have want
    where
    loc = inferenceLocation program

data Infinitude = Finite | Infinite

unificationError
  :: forall (a :: Kind)
  . (DiagnosticLocations a, ReifyKind a, TidyType a, ToText (Type a))
  => Infinitude
  -> Location
  -> Type a
  -> Type a
  -> [ErrorGroup]
unificationError finitude location have want = runTidy $ do
  have' <- tidyType have
  want' <- tidyType want
  return . (:[]) . ErrorGroup
    $ (CompileError location Error . T.unwords)
      [ "expected"
      , toText kind
      , "type"
      , toText want'
      , "but got"
      , toText have'
      ]
    : map locationInfo
      (diagnosticLocations want' ++ diagnosticLocations have')
    ++ case finitude of
      Finite -> []
      Infinite -> (:[]) $ CompileError location Note
        "maybe you gave a function of the wrong type to a combinator"
  where
  kind = reifyKind (KindProxy :: KindProxy a)
  locationInfo (loc, type_) = CompileError loc Note
    $ T.unwords [type_, "is from here"]

-- | Unifies two types, returning the second type.
unifyM
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> K (Type a)
unifyM have want = do
  program <- getsProgram $ unify have want
  case program of
    Right program' -> putProgram program' >> return want
    Left errors -> liftFailWriter $ throwMany errors

unifyM_
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> K ()
unifyM_ = (void .) . unifyM

shouldBe
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> K ()
shouldBe = unifyM_
infix 3 `shouldBe`

unifyVar
  :: forall a.
  ( Declare a
  , DiagnosticLocations a
  , Occurrences a
  , ReifyKind a
  , Substitute a
  , TidyType a
  , ToText (Type a)
  )
  => KindedId a
  -> Type a
  -> Program
  -> Either [ErrorGroup] Program
unifyVar var1 type_ program = case type_ of
  TyVar var2 _ | var1 == var2 -> return program
  TyVar{} -> return $ declare var1 type_ program
  _ | occurs (unkinded var1) program type_
    -> let loc = inferenceLocation program in Left $ unificationError
      Infinite loc
      (sub program (TyVar var1 loc :: Type a))
      (sub program type_)
  _ -> return $ declare var1 type_ program
