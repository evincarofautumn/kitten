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
import Kitten.Util.Either
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
    (a :. b, c :. d) -> withContext' $ unify b d program >>= unify a c
    (TyVar var (Origin hint _), type_)
      -> withContext' $ unifyVar var (type_ `addHint` hint) program
    (type_, TyVar var (Origin hint _))
      -> withContext' $ unifyVar var (type_ `addHint` hint) program
    _ -> Left $ unificationError Finite loc have want
    where
    withContext' = withContext have want loc
    loc = originLocation $ inferenceOrigin program

instance Unification Scalar where
  unification have want program = case (have, want) of
    _ | have == want -> Right program
    (a :& b, c :& d) -> withContext' $ unify b d program >>= unify a c
    ((:?) a, (:?) b) -> withContext' $ unify a b program
    (a :@ as, b) | V.null as -> unify a b program
    (a, b :@ bs) | V.null bs -> unify a b program
    (a :@ as, b :@ bs) -> withContext' $ do
      program' <- unify a b program
      if V.length as /= V.length bs
        then Left $ unificationError Finite loc have want
        else V.foldM (\p (x, y) -> unify x y p) program' $ V.zip as bs
    (a :| b, c :| d) -> withContext' $ unify b d program >>= unify a c
    (TyFunction a b _, TyFunction c d _)
      -> withContext' $ unify b d program >>= unify a c
    (TyVector a _, TyVector b _) -> withContext' $ unify a b program
    (TyVar var (Origin hint _), type_)
      -> withContext' $ unifyVar var (type_ `addHint` hint) program
    (type_, TyVar var (Origin hint _))
      -> withContext' $ unifyVar var (type_ `addHint` hint) program
    (TyQuantified scheme loc', type_) -> withContext' $ let
      (type', program') = instantiate loc' scheme program
      in unify type' type_ program'
    (type_, TyQuantified scheme loc') -> withContext' $ let
      (type', program') = instantiate loc' scheme program
      in unify type' type_ program'
    _ -> Left $ unificationError Finite loc have want
    where
    withContext' = withContext have want loc
    loc = originLocation $ inferenceOrigin program

withContext
  :: forall (a :: Kind) b
  . (DiagnosticLocations a, ReifyKind a, TidyType a, ToText (Type a))
  => Type a -> Type a -> Location
  -> Either [ErrorGroup] b -> Either [ErrorGroup] b
withContext have want loc
  = mapLeft (++ unificationError Finite loc have want)

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
  let
    primaryError = CompileError location Error $ T.unwords
      [ "expected"
      , toText kind
      , "type"
      , toText want'
      , "but got"
      , toText have'
      ]
    secondaryErrors = map errorDetail
      $ diagnosticLocations have' ++ diagnosticLocations want'
    tertiaryErrors = case finitude of
      Finite -> []
      Infinite -> (:[]) $ CompileError location Note
        "this typically indicates a wrong number of arguments to a function"
  return [ErrorGroup (primaryError : secondaryErrors ++ tertiaryErrors)]
  where
  kind = reifyKind (KindProxy :: KindProxy a)
  errorDetail (origin@(Origin _ loc), type_) = CompileError loc Note $ T.concat
    [ type_
    , originSuffix origin
    , " is from here"
    ]

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
    -> let loc = originLocation $ inferenceOrigin program in Left $ unificationError
      Infinite loc
      (sub program (TyVar var1 (Origin HiNone loc) :: Type a))
      (sub program type_)
  _ -> return $ declare var1 type_ program
