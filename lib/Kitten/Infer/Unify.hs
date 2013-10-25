{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Error
import Kitten.Infer.Locations
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Location
import Kitten.Type
import Kitten.Util.FailWriter
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

instance Unification Effect where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (NoEffect loc, a :+ b)
      -> unify a (NoEffect loc) env
      >>= unify b (NoEffect loc)

    (a :+ b, NoEffect loc)
      -> unify a (NoEffect loc) env
      >>= unify b (NoEffect loc)

    (a :+ b, c :+ d) | a +: b == c +: d -> Right env
    (a :+ b, c :+ d) -> unify a c env >>= unify b d

    (Var var _, type_) -> unifyVar var type_ env
    (type_, Var var _) -> unifyVar var type_ env

    _ -> Left $ unificationError "effect"
      (envLocation env) type1 type2

unificationError
  :: (ToText (Type a))
  => Text
  -> Location
  -> Type a
  -> Type a
  -> [ErrorGroup]
unificationError kind location type1 type2 = (:[]) . ErrorGroup
  $ (CompileError location Error . T.unwords)
    [ "cannot solve", kind, "type constraint"
    , toText type1
    , "="
    , toText type2
    ]
  : map errorDetail (locations type1 ++ locations type2)
  where
  errorDetail (loc, type_) = CompileError loc Note
    $ toText type_ <> " is from here"

instance Unification Row where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (a :. b, c :. d) -> unify b d env >>= unify a c

    (Var var _, type_) -> unifyVar var type_ env
    (type_, Var var _) -> unifyVar var type_ env

    _ -> Left $ unificationError "row"
      (envLocation env) type1 type2

instance Unification Scalar where
  unification type1 type2 env = case (type1, type2) of
    _ | type1 == type2 -> Right env

    (a :& b, c :& d) -> unify b d env >>= unify a c
    ((:?) a, (:?) b) -> unify a b env
    (a :| b, c :| d) -> unify b d env >>= unify a c

    (Function a b e1 _, Function c d e2 _)
      -> unify b d env >>= unify a c >>= unify e1 e2

    (Vector a _, Vector b _) -> unify a b env

    (Var var _, type_) -> unifyVar var type_ env
    (type_, Var var _) -> unifyVar var type_ env

    _ -> Left $ unificationError "scalar"
      (envLocation env) type1 type2

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
    (Function a b p1 _, Function c d p2 _)
      -> p1 == p2 && a <: c && b <: d
    (Vector a _, Vector b _) -> a <: b
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
  :: forall a. (Declare a, Occurrences a, Substitute a, ToText (Type a))
  => TypeName a
  -> Type a
  -> Env
  -> Either [ErrorGroup] Env
unifyVar var1 type_ env = case type_ of
  Var var2 _ | var1 == var2 -> return env
  Var{} -> return $ declare var1 type_ env
  _ | occurs (unTypeName var1) env type_
    -> let loc = envLocation env in Left
      $ unificationError "infinite" loc
        (sub env (Var var1 UnknownLocation :: Type a))
        (sub env type_)
  _ -> return $ declare var1 type_ env
