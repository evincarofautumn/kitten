{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Declare(..)
  , freshConst
  , freshM
  , freshVar
  , freshConstM
  , freshVarM
  , retrieve
  , withLocation
  , withOrigin
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Error
import Kitten.Location
import Kitten.Id
import Kitten.K
import Kitten.Type
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

import qualified Kitten.IdMap as Id

class Declare a where
  declare :: TypeId a -> Type a -> Inference -> Inference

instance Declare Stack where
  declare (TypeId i) type_ env = env
    { inferenceStacks = Id.insert i type_ (inferenceStacks env) }

instance Declare Scalar where
  declare (TypeId i) type_ env = env
    { inferenceScalars = Id.insert i type_ (inferenceScalars env) }

class Retrieve a where
  retrieve :: Inference -> TypeId a -> Either ErrorGroup (Type a)

instance Retrieve Stack where
  retrieve Inference{..} (TypeId name)
    = flip maybeToEither (Id.lookup name inferenceStacks)
    $ nonexistent "stack" inferenceOrigin name

instance Retrieve Scalar where
  retrieve Inference{..} (TypeId name)
    = flip maybeToEither (Id.lookup name inferenceScalars)
    $ nonexistent "scalar" inferenceOrigin name

nonexistent :: Text -> Origin -> Id n -> ErrorGroup
nonexistent kind (Origin _ loc) name
  = oneError $ CompileError loc Error $ T.concat
  [ "nonexistent ", kind, " variable '"
  , toText name
  , "'"
  ]

fresh
  :: forall a (b :: Kind -> *) block
  . (TypeId a -> Origin -> b a)
  -> Origin
  -> Program block
  -> (b a, Program block)
fresh constructor origin env
  = let (typeId, env') = freshTypeId env
  in (constructor (TypeId typeId) origin, env')

freshConst :: Origin -> Program block -> (Type a, Program block)
freshConst = fresh Const

freshVar :: Origin -> Program block -> (Type a, Program block)
freshVar = fresh Var

freshConstM :: K block (Type a)
freshConstM = freshM freshConst

freshVarM :: forall (a :: Kind) block. K block (Type a)
freshVarM = freshM freshVar

withLocation :: Location -> K block a -> K block a
withLocation here = withOrigin (Origin NoHint here)

withOrigin :: Origin -> K block a -> K block a
withOrigin here action = do
  there <- getsProgram (inferenceOrigin . programInference)
  modifyInference $ \env -> env { inferenceOrigin = here }
  result <- action
  modifyInference $ \env -> env { inferenceOrigin = there }
  return result
