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

-- import Control.Applicative
import Control.Monad
import Data.Function
import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Error
import Kitten.Infer.Locations
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Location
import Kitten.Type.Tidy
import Kitten.Types
import Kitten.Util.Either
import Kitten.Util.FailWriter
import Kitten.Util.Maybe
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
    _ -> Left $ unificationError Nothing loc have want
    where
    withContext' = withContext have want loc
    loc = originLocation $ inferenceOrigin program

instance Unification Scalar where
  unification have want program = case (have, want) of
    _ | have == want -> Right program
    (a :& b, c :& d) -> withContext' $ unify b d program >>= unify a c
    ((:?) a, (:?) b) -> withContext' $ unify a b program
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
    _ -> Left $ unificationError Nothing loc have want
    where
    withContext' = withContext have want loc
    loc = originLocation $ inferenceOrigin program

withContext
  :: forall (a :: Kind) b
  . (DiagnosticLocations a, ReifyKind a, TidyType a, ToText (Type a))
  => Type a -> Type a -> Location
  -> Either [ErrorGroup] b -> Either [ErrorGroup] b
withContext have want loc
  = mapLeft (++ unificationError Nothing loc have want)

{-
[ErrorGroup [CompileError loc Note
  (let (have', want') = runTidy $ (liftA2 (,) `on` tidyType) have want
  in T.unwords
  $ "expected"
  : toText (reifyKind (KindProxy :: KindProxy a))
  : "type"
  : toText want'
  : "but got"
  : toText have'
  : [])]]
-}

unificationError
  :: forall (a :: Kind)
  . (DiagnosticLocations a, ReifyKind a, TidyType a, ToText (Type a))
  => Maybe Text
  -> Location
  -> Type a
  -> Type a
  -> [ErrorGroup]
unificationError prefix location have want = runTidy $ do
  have' <- tidyType have
  want' <- tidyType want
  let
    primaryError = CompileError location Error $ T.unwords
      $ "expected"
      : prefix `consMaybe` toText kind
      : "type"
      : toText want'
      : "but got"
      : toText have'
      : []
    secondaryErrors = map errorDetail
      $ diagnosticLocations have' ++ diagnosticLocations want'
  return [ErrorGroup (primaryError : secondaryErrors)]
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
      (Just "infinite") loc
      (sub program (TyVar var1 (Origin HiNone loc) :: Type a))
      (sub program type_)
  _ -> return $ declare var1 type_ program
