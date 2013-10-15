{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Infer.Unify
  ( (===)
  , unifyM
  , unifyM_
  , unifyVar
  ) where

import Control.Monad
import Data.Function
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Error
import Kitten.Infer.Locations
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Location
import Kitten.Type
import Kitten.Type.Tidy
import Kitten.Util.FailWriter
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

-- | Simplifies and unifies two types.
unify
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Env
  -> Either [ErrorGroup] Env
unify a b env = (unification `on` simplify env) a b env

class Unification a where
  unification
    :: Type a
    -> Type a
    -> Env
    -> Either [ErrorGroup] Env

instance Unification Row where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (a :. b, c :. d) -> unify b d env >>= unify a c

    (Var var (Origin hint _), type_) -> unifyVar var (type_ `addHint` hint) env
    (_, Var{}) -> commutative

    _ -> Left $ unificationError Nothing
      (envLocation env) type1 type2

    where commutative = unification type2 type1 env

instance Unification Scalar where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env
    (a :& b, c :& d) -> unify b d env >>= unify a c
    ((:?) a, (:?) b) -> unify a b env
    (a :| b, c :| d) -> unify b d env >>= unify a c
    (Function a b _, Function c d _) -> unify b d env >>= unify a c
    (Vector a _, Vector b _) -> unify a b env
    (Var var (Origin hint _), type_) -> unifyVar var (type_ `addHint` hint) env
    (_, Var{}) -> commutative
    (Quantified scheme loc, _) -> let
      (type', env') = instantiate loc scheme env
      in unify type' type2 env'
    (_, Quantified{}) -> commutative

    _ -> Left $ unificationError Nothing
      (envLocation env) type1 type2

    where commutative = unification type2 type1 env

unificationError
  :: forall (a :: Kind). (ReifyKind a, TidyType a, ToText (Type a))
  => Maybe Text
  -> Location
  -> Type a
  -> Type a
  -> [ErrorGroup]
unificationError prefix location type1 type2 = runTidy $ do
  type1' <- tidyType type1
  type2' <- tidyType type2
  let
    primaryError = CompileError location Error $ T.unwords
      $ "cannot solve"
      : prefix `consMaybe` toText kind
      : "type constraint"
      : toText type1
      : "="
      : toText type2
      : []
    secondaryErrors = map errorDetail
      $ diagnosticLocations type1' ++ diagnosticLocations type2'
  return [ErrorGroup (primaryError : secondaryErrors)]
  where
  kind = reifyKind (KindProxy :: KindProxy a)
  errorDetail (loc, type_) = CompileError loc Note
    $ toText type_ <> " is from here"

-- | Unifies two types, returning the second type.
unifyM
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Inferred (Type a)
unifyM type1 type2 = do
  env <- getsEnv $ unify type1 type2
  case env of
    Right env' -> putEnv env' >> return type2
    Left errors -> liftFailWriter $ throwMany errors

unifyM_
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Inferred ()
unifyM_ = (void .) . unifyM

(===)
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Inferred ()
(===) = unifyM_

infix 3 ===

unifyVar
  :: forall a.
  ( Declare a
  , Occurrences a
  , ReifyKind a
  , Substitute a
  , TidyType a
  , ToText (Type a)
  )
  => TypeName a
  -> Type a
  -> Env
  -> Either [ErrorGroup] Env
unifyVar var1 type_ env = case type_ of
  Var var2 _ | var1 == var2 -> return env
  Var{} -> return $ declare var1 type_ env
  _ | occurs (unTypeName var1) env type_
    -> let loc = envLocation env in Left $ unificationError
      (Just "infinite") loc
      (sub env (Var var1 (Origin NoHint UnknownLocation) :: Type a))
      (sub env type_)
  _ -> return $ declare var1 type_ env
