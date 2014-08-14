{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Declare(..)
  , Fresh(..)
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
import Kitten.Types
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

import qualified Kitten.IdMap as Id

class Declare a where
  declare :: KindedId a -> Type a -> Program -> Program

instance Declare Stack where
  declare (KindedId i) type_ program = program
    { inferenceStacks = Id.insert i type_ (inferenceStacks program) }

instance Declare Scalar where
  declare (KindedId i) type_ program = program
    { inferenceScalars = Id.insert i type_ (inferenceScalars program) }

class Retrieve a where
  retrieve :: Program -> KindedId a -> Either ErrorGroup (Type a)

instance Retrieve Stack where
  retrieve Program{..} (KindedId name)
    = flip maybeToEither (Id.lookup name inferenceStacks)
    $ nonexistent "stack" inferenceOrigin name

instance Retrieve Scalar where
  retrieve Program{..} (KindedId name)
    = flip maybeToEither (Id.lookup name inferenceScalars)
    $ nonexistent "scalar" inferenceOrigin name

nonexistent :: Text -> Origin -> Id n -> ErrorGroup
nonexistent kind (Origin _ loc) name
  = oneError $ CompileError loc Error $ T.concat
  [ "nonexistent ", kind, " variable '"
  , toText name
  , "'"
  ]

class Fresh (a :: Kind) where
  fresh
    :: forall (b :: Kind -> *)
    . (KindedId a -> Origin -> b a)
    -> Origin -> Program -> (b a, Program)

instance Fresh Scalar where
  fresh constructor origin program
    = let (typeId, program') = freshScalarId program
    in (constructor typeId origin, program')

instance Fresh Stack where
  fresh constructor origin program
    = let (typeId, program') = freshStackId program
    in (constructor typeId origin, program')

freshConst
  :: forall (a :: Kind). (Fresh a) => Origin -> Program -> (Type a, Program)
freshConst = fresh TyConst

freshVar
  :: forall (a :: Kind). (Fresh a) => Origin -> Program -> (Type a, Program)
freshVar = fresh TyVar

freshConstM :: forall (a :: Kind). (Fresh a) => K (Type a)
freshConstM = freshM freshConst

freshVarM :: forall (a :: Kind). (Fresh a) => K (Type a)
freshVarM = freshM freshVar

withLocation :: Location -> K a -> K a
withLocation here = withOrigin (Origin HiNone here)

withOrigin :: Origin -> K a -> K a
withOrigin here action = do
  there <- getsProgram inferenceOrigin
  modifyProgram $ \program -> program { inferenceOrigin = here }
  result <- action
  modifyProgram $ \program -> program { inferenceOrigin = there }
  return result
