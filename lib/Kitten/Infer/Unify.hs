{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Infer.Unify
  ( (<:)
  , (===)
  , unifyM
  , unifyM_
  , unifyVar
  ) where

import Control.Monad
import Data.Function

import Kitten.Error
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Type
import Kitten.Util.FailWriter

-- | Simplifies and unifies two types.
unify
  :: (Unification a, Simplify a)
  => Type a
  -> Type a
  -> Env
  -> Either CompileError Env
unify a b env = (unification `on` simplify env) a b env

class Unification a where
  unification
    :: Type a
    -> Type a
    -> Env
    -> Either CompileError Env

instance Unification Effect where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env
    (a :+ b, c :+ d) | a +: b == c +: d -> Right env

    (Var var, type_) -> unifyVar (effect var) type_ env
    (type_, Var var) -> unifyVar (effect var) type_ env

    _ -> Left . TypeError (envLocation env) $ unwords
      [ "cannot solve effect type constraint:"
      , show type1
      , "="
      , show type2
      ]

instance Unification Row where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (a :. b, c :. d) -> unify b d env >>= unify a c

    (Var var, type_) -> unifyVar (row var) type_ env
    (type_, Var var) -> unifyVar (row var) type_ env

    _ -> Left . TypeError (envLocation env) $ unwords
      [ "cannot solve row type constraint:"
      , show type1
      , "="
      , show type2
      ]

instance Unification Scalar where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (a :& b, c :& d) -> unify b d env >>= unify a c
    ((:?) a, (:?) b) -> unify a b env
    (a :| b, c :| d) -> unify b d env >>= unify a c

    (Function a b e1, Function c d e2)
      -> unify b d env >>= unify a c >>= unify e1 e2

    (Vector a, Vector b) -> unify a b env

    (Var var, type_) -> unifyVar (scalar var) type_ env
    (type_, Var var) -> unifyVar (scalar var) type_ env

    _ -> Left . TypeError (envLocation env) $ unwords
      [ "cannot solve scalar type constraint:"
      , show type1
      , "="
      , show type2
      ]

class Subtype a where
  (<:) :: Type a -> Type a -> Bool

instance Subtype Row where
  type1 <: type2 = case (type1, type2) of
    _ | type1 == type2 -> True
    (a :. b, c :. d) -> b <: d && a <: c
    (Var{}, Var{}) -> False
    (Var{}, _) -> True
    _ -> False

instance Subtype Scalar where
  type1 <: type2 = case (type1, type2) of
    _ | type1 == type2 -> True
    (a :& b, c :& d) -> b <: d && a <: c
    (Function a b p1, Function c d p2)
      -> p1 == p2 && a <: c && b <: d
    (Vector a, Vector b) -> a <: b
    (Var{}, Var{}) -> False
    (Var{}, _) -> True
    _ -> False

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
    Left err -> Inferred $ throwMany [err]

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
  :: forall a. (Declare a, Occurrences a, Substitute a)
  => TypeName a
  -> Type a
  -> Env
  -> Either CompileError Env
unifyVar var1 type_ env = case type_ of
  Var var2 | typeName var1 == var2 -> return env
  Var{} -> return $ declare var1 type_ env
  _ | occurs (typeName var1) env type_
    -> Left . TypeError (envLocation env) $ unwords
      [ "cannot construct infinite type"
      , show $ sub env (Var (typeName var1) :: Type a)
      , "="
      , show $ sub env type_
      ]
  _ -> return $ declare var1 type_ env
